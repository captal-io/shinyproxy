#!/bin/bash

server="ubuntu@ec2-18-230-164-232.sa-east-1.compute.amazonaws.com"
pem="shinyproxy"

printf "\n\n\t(1) send files to server, (2) connect to server or (3) install docker and docker-compose"
printf "\n\t\t~ "
read opt

if [ $opt == 1 ]
then
    rsync -rv --exclude=$pem.pem --exclude=.git . deploy
    scp -i $pem.pem -r deploy/ $server:/home/ubuntu/
    sudo rm -rf deploy/
    ssh -i $pem.pem $server
    
elif [ $opt == 2 ]
then
    printf "\n\nConnecting to server...\n\n" 

    ssh -i $pem.pem $server
elif [ $opt == 3 ]
then
    # https://docs.docker.com/engine/install/ubuntu/

    # docker
    sudo apt-get update
    sudo apt-get install \
        apt-transport-https \
        ca-certificates \
        curl \
        gnupg \
        lsb-release -y
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
    echo \
    "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
    $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
    sudo apt-get update
    sudo apt-get install docker-ce docker-ce-cli containerd.io -y

    # compose
    sudo curl -L "https://github.com/docker/compose/releases/download/1.29.2/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
     sudo chmod +x /usr/local/bin/docker-compose
     sudo ln -s /usr/local/bin/docker-compose /usr/bin/docker-compose
else
    printf "Try again"
fi
