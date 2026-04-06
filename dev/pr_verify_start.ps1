param(
  [Parameter(Mandatory = $true)][string]$Topic,
  [Parameter(Mandatory = $true)][int]$Port
)
$ErrorActionPreference = "Stop"
Set-Location "C:\Rprojects\teal.modules.clinical"
Get-NetTCPConnection -LocalPort $Port -ErrorAction SilentlyContinue | ForEach-Object {
  Stop-Process -Id $_.OwningProcess -Force -ErrorAction SilentlyContinue
}
Copy-Item -Force "C:\temp\tmc_pr_helpers\*" "dev\"
New-Item -ItemType Directory -Force "inst\pr_verification" | Out-Null
& "C:\Program Files\R\R-devel\bin\Rscript.exe" "dev\extract_rd_examples.R" $Topic | Out-File -Encoding utf8 "_topic_code.txt"
Start-Process -FilePath "C:\Program Files\R\R-devel\bin\Rscript.exe" -ArgumentList @("dev/run_example_in_browser.R", $Topic, "$Port") -WindowStyle Hidden
Start-Sleep -Seconds 45
(Invoke-WebRequest -Uri "http://127.0.0.1:$Port/" -UseBasicParsing -TimeoutSec 90).StatusCode
