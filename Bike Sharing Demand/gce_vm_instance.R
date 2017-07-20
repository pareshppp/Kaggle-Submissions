library(googleComputeEngineR)

# Setup
project_id <- 'rstudio-paresh'
zone <- 'asia-southeast1-a'
auth_file <- '~/GCE_ServiceAccount_Key/Rstudio - Paresh/rstudio-paresh-key.json'

Sys.setenv(GCE_DEFAULT_PROJECT_ID = project_id,
           GCE_DEFAULT_ZONE = zone,
           GCE_AUTH_FILE = auth_file)

gce_auth()



# Set default Project & Zone
gce_global_project(project = project_id)
gce_global_zone(zone = zone)

default_project <- gce_get_project()
default_project$name



# VM Instance
rstudio_vm <- gce_vm(template = 'rstudio-hadleyverse', 
             name = 'rstudio-instance', 
             username = 'rstudio',
             password = 'r1992',
             predefined_type = 'n1-highmem-4')

rstudio_vm$status

# Start Stop VM
gce_vm_stop(rstudio_vm)
gce_vm_start(rstudio_vm)


gce_list_instances()
