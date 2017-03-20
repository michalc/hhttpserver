terraform {
  backend "s3" {
    bucket = "terraform.charemza.name"
    key    = "hhttpserver.tfstate"
    region = "eu-west-1"
    profile = "hhttpserver"
    lock_table = "hhttpserver_infrastructure"
  }
}

provider "aws" {
  region = "eu-west-1"
  profile = "hhttpserver"
}

module "infrastructure" {
  source = "./infrastructure"
}
