param(
    [string]$AppDataPath = $env:APPDATA
)
$ErrorActionPreference = 'Stop'

# Copy only launchers; credentials, settings, and history stay on this machine.
$files = @(
    'npm\claude-bedrock.cmd',
    'npm\claude-pro.cmd',
    'claude-profiles\launch.ps1'
)
foreach ($file in $files) {
    $source = Join-Path (Join-Path $PSScriptRoot 'AppData\Roaming') $file
    $target = Join-Path $AppDataPath $file
    New-Item -ItemType Directory -Force (Split-Path -Parent $target) | Out-Null
    if (Test-Path -LiteralPath $target) {
        if ((Get-FileHash -LiteralPath $source).Hash -eq (Get-FileHash -LiteralPath $target).Hash) {
            continue
        }
        $backup = $target + '.backup.' + [guid]::NewGuid().ToString('N')
        Copy-Item -LiteralPath $target -Destination $backup
        Write-Output "Previous launcher saved to $backup"
    }
    Copy-Item -LiteralPath $source -Destination $target
}
Write-Output "Claude launchers installed in $AppDataPath. Ensure $AppDataPath\npm is on PATH."
