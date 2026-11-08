param(
 [Parameter(Mandatory=$true)][ValidateSet('pro','bedrock')][string]$Mode,
 [string]$Profile,
 [string]$Region,
 [Parameter(ValueFromRemainingArguments=$true)][string[]]$ClaudeArgs
)
$ErrorActionPreference='Stop'
if ($Mode -eq 'bedrock' -and (!$Profile -or !$Region)) { throw 'Usage: claude-bedrock -Profile <AWS profile> -Region <AWS region> [Claude arguments]' }
Get-ChildItem Env: | Where-Object { $_.Name -like 'ANTHROPIC_*' -or $_.Name -like 'CLAUDE_CODE_USE_*' -or $_.Name -in @('CLAUDE_CODE_OAUTH_TOKEN','CLAUDE_CODE_OAUTH_TOKEN_FILE_DESCRIPTOR','CLAUDE_CODE_API_KEY_FILE_DESCRIPTOR','CLAUDE_CODE_MODEL') } | ForEach-Object { Remove-Item -LiteralPath ('Env:' + $_.Name) }
$env:CLAUDE_CONFIG_DIR=Join-Path $env:APPDATA ('claude-profiles\' + $Mode)
New-Item -ItemType Directory -Force $env:CLAUDE_CONFIG_DIR | Out-Null
if ($Mode -eq 'bedrock') {
 $env:CLAUDE_CODE_USE_BEDROCK='1'
 $env:AWS_PROFILE=$Profile
 $env:AWS_REGION=$Region
 $env:AWS_DEFAULT_REGION=$Region
 @('AWS_ACCESS_KEY_ID','AWS_SECRET_ACCESS_KEY','AWS_SESSION_TOKEN','AWS_BEARER_TOKEN_BEDROCK') | ForEach-Object { Remove-Item -LiteralPath ('Env:' + $_) -ErrorAction SilentlyContinue }
}
& (Join-Path $env:APPDATA 'npm\claude.cmd') @ClaudeArgs
exit $LASTEXITCODE
