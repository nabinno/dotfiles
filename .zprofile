if [[ ! -f /run/screen ]]; then sudo mkdir -p /run/screen; fi
if [ "$(stat --format '%a' /run/screen)" -eq 777 ]; then sudo chmod 777 /run/screen; fi
