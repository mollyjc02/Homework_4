if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)



for (y in 2010:2015) {

# read contract information 
contract_path <- paste0("data/input/monthly-ma-and-pdp-enrollment-by-cpsc/CPSC_Contract_Info_",y,"_01.csv")

contract_info <- read_csv(contract_path,
                          skip = 1,
                          col_names = c("contractid", "planid",
                                        "org_type", "plan_type", "partd", "snp",
                                        "eghp", "org_name",
                                        "org_marketing_name", "plan_name",
                                        "parent_org", "contract_date"),
                          col_types = cols(
                            contractid = col_character(),
                            planid = col_double(),
                            org_type = col_character(),
                            plan_type = col_character(),
                            partd = col_character(),
                            snp = col_character(),
                            eghp = col_character(),
                            org_name = col_character(),
                            org_marketing_name = col_character(),
                            plan_name = col_character(),
                            parent_org = col_character(),
                            contract_date = col_character()
                          ))
  
  
  contract_info = contract_info %>%
    group_by(contractid, planid) %>%
    dplyr::mutate(id_count=row_number())%>%
  filter(id_count == 1) %>%
  select(-id_count)


contract_info <- contract_info %>% filter(!is.na(planid)) #### CHECK THIS 



# read enrollment information 
enrollment_path = paste0("data/input/monthly-ma-and-pdp-enrollment-by-cpsc/CPSC_Enrollment_Info_",y,"_01.csv")

enroll_info = read_csv(enrollment_path,
                          skip = 1,
                          col_names = c("contractid", "planid", "ssa",
                                        "fips", "state", "county", "enrollment"),
                          col_types = cols(
                            contractid = col_character(),
                            planid = col_double(),
                            ssa = col_double(),
                            fips = col_double(),
                            state = col_character(),
                            county = col_character(),
                            enrollment = col_double()
                          ), na = "*")

enroll_info <- enroll_info %>% #### should I be worried about amnt of NAs
  filter(!is.na(fips))


 ### merge contract info with enrollment info
  plan_data = contract_info %>%
    left_join(enroll_info, by=c("contractid", "planid")) %>%
    dplyr::mutate(year=y)
    
  ### fill in missing fips codes (by state and county)
  plan_data = plan_data %>%
    group_by(state, county) %>%
    fill(fips)

  ### fill in missing plan characteristics by contract and plan id
  plan_data = plan_data %>%
    group_by(contractid, planid) %>%
    fill(plan_type, partd, snp, eghp, plan_name)
  
  ### fill in missing contract characteristics by contractid
  plan_data = plan_data %>%
    group_by(contractid) %>%
    fill(org_type,org_name,org_marketing_name,parent_org)


### collapse from monthly data to yearly
  plan.year = plan_data %>%
    group_by(contractid, planid, fips) %>%
    arrange(contractid, planid, fips) 

write_rds(plan.year,paste0("data/output/ma_data_",y,".rds"))

}

full.ma.data <- read_rds("data/output/ma_data_2010.rds")

for (y in 2011:2015) {
  full.ma.data <- rbind(full.ma.data,read_rds(paste0("data/output/ma_data_",y,".rds")))
}
