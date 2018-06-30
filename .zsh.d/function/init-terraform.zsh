export REQUIRED_TERRAFORM_VERSION=0.6.6

get-terraform() {
  local current_pwd=$(pwd)
  cd ~/.local/bin
  rm -fr terraform*
  case "${OSTYPE}" in
    darwin*)
      wget https://releases.hashicorp.com/terraform/${REQUIRED_TERRAFORM_VERSION}/terraform_${REQUIRED_TERRAFORM_VERSION}_darwin_amd64.zip
      unzip terraform_${REQUIRED_TERRAFORM_VERSION}_darwin_amd64.zip
      ;;
    freebsd*)
      wget https://releases.hashicorp.com/terraform/${REQUIRED_TERRAFORM_VERSION}/terraform_${REQUIRED_TERRAFORM_VERSION}_freebsd_amd64.zip
      unzip terraform_${REQUIRED_TERRAFORM_VERSION}_freebsd_amd64.zip
      ;;
    linux*)
      wget https://releases.hashicorp.com/terraform/${REQUIRED_TERRAFORM_VERSION}/terraform_${REQUIRED_TERRAFORM_VERSION}_linux_amd64.zip
      unzip terraform_${REQUIRED_TERRAFORM_VERSION}_linux_amd64.zip
      ;;
  esac
  cd $current_pwd
}
if ! type -p terraform >/dev/null; then get-terraform; fi

terraform-remote-config() {
  terraform remote config -backend=S3 -backend-config="bucket=tfstate.d" -backend-config="key=$1.tfstate"
  terraform remote push
}

alias tfrc="terraform-remote-config"
alias tfr="terraform remote"
alias tfs="terraform show"
alias tfp="terraform plan"
alias tfa="terraform apply"
alias tfd="terraform destroy"
