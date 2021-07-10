# == INDEX
# Base::EnvironmentVariable
# PackageManagement::WindowsPackageManagement
# PackageManagement::WindowsPackageManagement::Chocolatey
# PackageManagement::Docker
# ProgrammingLanguage::DotNet
# ProgrammingLanguage::DotNet::UniversalWindowsPlatform
# ProgrammingLanguage::Shell
# ProgrammingLanguage::Go
# IntegratedDevelopmentEnvironment::Editor::Emacs
# IntegratedDevelopmentEnvironment::Editor::Vi
# IntegratedDevelopmentEnvironment::ResourceManagement::Git
# IntegratedDevelopmentEnvironment::Network
# IntegratedDevelopmentEnvironment::OsLevelVirtualization::WSL
# IntegratedDevelopmentEnvironment::OsLevelVirtualization::HyperV
# IntegratedDevelopmentEnvironment::IoT
# Other


# Base::EnvironmentVariable


# PackageManagement::WindowsPackageManagement
Set-Alias gpkg Get-Package
Set-Alias fpkg Find-Package


# PackageManagement::WindowsPackageManagement::Chocolatey
if (!(Get-Command -ErrorAction Ignore choco)) {
    Set-ExecutionPolicy Bypass -Scope Process -Force; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
}
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
    Import-Module "$ChocolateyProfile"
}


# PackageManagement::Docker
function dp { docker ps -a }
function dl { docker images -a }


# ProgrammingLanguage::DotNet
function Set-AspNetProject {
    npm install yo -g
    npm install generator-aspnet -g
    yo aspnet
    # dnx . run
}
function Install-OmniSharp {
    git clone https://github.com/OmniSharp/omnisharp-server.git
    cd omnisharp-server
    git submodule update --init --recursive
    msbuild
    cd ..
    mv omnisharp-server ~/OneDrive/bin/
}


# ProgrammingLanguage::DotNet::UniversalWindowsPlatform
function Set-AppXPackages {
  Get-AppxPackage | foreach { Add-AppxPackage -DisableDevelopmentMode -Register "$($_.InstallLocation)\AppXManifest.xml" }
}


# ProgrammingLanguage::Shell
Set-Alias bash 'C:\Windows\System32\bash.exe'


# ProgrammingLanguage::Go
$env:GOROOT ='C:\Program Files\Go'
$env:GOPATH = "$([Environment]::GetFolderPath('UserProfile'))\.ghq.d"
$env:GO111MODULE = 'on'
$env:PATH = "$($env:GOROOT)\bin;$($env:PATH)"
$env:PATH = "$($env:GOPATH)\bin;$($env:PATH)"
function Get-Go {
    choco install golang
    go get -u github.com/peco/peco/cmd/peco
    go get -u github.com/x-motemen/ghq
}
if (!(Get-Command -ErrorAction Ignore "$($env:GOROOT)\bin\go.exe")) { Get-Go }


# IntegratedDevelopmentEnvironment::Editor::Emacs
if (!(Get-Command -ErrorAction Ignore emacs)) { choco install emacs }
function f($file) { Start-Process -NoNewWindow emacs $file }
Set-PSReadlineOption -EditMode Emacs


# IntegratedDevelopmentEnvironment::Editor::Vi
$Vim = "C:\tools\vim\vim82\vim.exe"
Set-Alias vim $Vim
Set-Alias e $Vim
if (!(Get-Command -ErrorAction Ignore $Vim)) { choco install vim }


# IntegratedDevelopmentEnvironment::ResourceManagement::Git
$env:GITROOT ='C:\Program Files\Git'
$env:PATH = "$($env:GITROOT)\bin;$($env:PATH)"
function Get-Git {
    choco install git
}
if (!(Get-Command -ErrorAction Ignore "$($env:GITROOT)\bin\git.exe")) { Get-Git }
function ga($path) { git add $path }
function gaa { git add . }
function gct($comment) { git commit -m "$comment" }
function gd { git diff }
function gdf { git diff }
function glg { git log }
function gs { git status -sb }
Set-Alias g git


# IntegratedDevelopmentEnvironment::Network
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
function Edit-Hosts {
    Start-Process -NoNewWindow emacs $env:SystemRoot\System32\drivers\etc\hosts
}
function Start-PortForwarding {
    Start-Job -Name 'PortForwarding' -ScriptBlock {
        ssh -N -L 3000:localhost:3000 -L 443:localhost:443 vagrant@fe80::215:5dff:fe00:4100
    }
}
function Sync-HostsToWslIp {
    $hosts = "$env:SystemRoot\System32\drivers\etc\hosts";
    $pattern = "\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}";
    $wslip = bash.exe -c "ifconfig eth0 | grep 'inet '";
    if ($wslip -match $pattern) {
	$wslip = $matches[0];
    } else {
	echo "The Script Exited, the ip address of WSL 2 cannot be found";
	exit;
    }
    cat $hosts | %{ $_ -match $pattern }
    $rc = cat $hosts | %{ $_ -replace $matches[0], $wslip }
    $rc | Out-File $hosts;
}
Set-Alias eh Edit-Hosts
Set-Alias gna Get-NetAdapter
Set-Alias spf Start-PortForwarding
Set-Alias rna Restart-NetAdapter
Set-Alias shw Sync-HostsToWslIp


# IntegratedDevelopmentEnvironment::OsLevelVirtualization::WSL
if ((Get-WindowsOptionalFeature -Online -ErrorAction Ignore -FeatureName VirtualMachinePlatform).State -ne "Enabled") {
    Enable-WindowsOptionalFeature -Online -FeatureName VirtualMachinePlatform
}
if ((Get-WindowsOptionalFeature -Online -ErrorAction Ignore -FeatureName Microsoft-Windows-Subsystem-Linux).State -ne "Enabled") {
    Enable-WindowsOptionalFeature -Online -FeatureName Microsoft-Windows-Subsystem-Linux
}


# IntegratedDevelopmentEnvironment::OsLevelVirtualization::HyperV
function Connect-VM {
    [CmdletBinding(DefaultParameterSetName='name')]
    param(
        [Parameter(ParameterSetName='name')] [Alias('cn')] [System.String[]]$ComputerName=$env:COMPUTERNAME,
        [Parameter(Position=0, Mandatory, ValueFromPipelineByPropertyName, ValueFromPipeline, ParameterSetName='name')] [Alias('VMName')]      [System.String]$Name,
        [Parameter(Position=0, Mandatory, ValueFromPipelineByPropertyName, ValueFromPipeline, ParameterSetName='id')]   [Alias('VMId','Guid')] [System.Guid]$Id,
        # [Parameter(Position=0, Mandatory, ValueFromPipeline,                                  ParameterSetName='inputObject')] [Microsoft.HyperV.PowerShell.VirtualMachine]$InputObject,
        [switch]$StartVM
    )
    begin {
        Write-Verbose "Initializing InstanceCount, InstanceCount = 0"
        $InstanceCount=0
    } process {
        try {
            foreach($computer in $ComputerName) {
                Write-Verbose "ParameterSetName is '$($PSCmdlet.ParameterSetName)'"
                if ($PSCmdlet.ParameterSetName -eq 'name') {
                              # Get the VM by Id if Name can convert to a guid
                    if ($Name -as [guid]) {
                        Write-Verbose "Incoming value can cast to guid"
                        $vm = Get-VM -Id $Name -ErrorAction SilentlyContinue
                    } else {
                        $vm = Get-VM -Name $Name -ErrorAction SilentlyContinue
                    }
                } elseif ($PSCmdlet.ParameterSetName -eq 'id') {
                    $vm = Get-VM -Id $Id -ErrorAction SilentlyContinue
                } else {
                    $vm = $InputObject
                } if ($vm) {
                    Write-Verbose "Executing 'vmconnect.exe $computer $($vm.Name) -G $($vm.Id) -C $InstanceCount'"
                    vmconnect.exe $computer $vm.Name -G $vm.Id -C $InstanceCount
                } else {
                    Write-Verbose "Cannot find vm: '$Name'"
                } if ($StartVM -and $vm) {
                    if ($vm.State -eq 'off') {
                        Write-Verbose "StartVM was specified and VM state is 'off'. Starting VM '$($vm.Name)'"
                        Start-VM -VM $vm
                    } else {
                        Write-Verbose "Starting VM '$($vm.Name)'. Skipping, VM is not not in 'off' state."
                    }
                }
                $InstanceCount+=1
                Write-Verbose "InstanceCount = $InstanceCount"
            }
        } catch {
            Write-Error $_
        }
    }
}
Set-Alias gvm Get-VM
Set-Alias gvmn Get-VMNetworkAdapter


# IntegratedDevelopmentEnvironment::IoT
if (!(Get-Command -ErrorAction Ignore fwup)) { choco install fwup }


# Other
function d($path) { Remove-Item $path -Recurse -Force }
function re { vim $PROFILE }
function ree { Start-Process -NoNewWindow emacs $PROFILE }
function ll { Get-ChildItem -Exclude .* -Name }
function lf { Get-ChildItem -Exclude .* }
function la { Get-ChildItem -Exclude .*; Get-ChildItem -Hidden }
function ~ { Set-Location ~ }
function .. { Set-Location .. }
Set-Alias ggl google
Set-Alias i Get-Help
Set-Alias p Write-Output
Set-Alias ip Get-Process
Set-Alias j Set-Location
Set-Alias v Get-Content
function which($command) { (Get-Command $command).Source }
