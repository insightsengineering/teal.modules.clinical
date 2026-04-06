param(
  [Parameter(Mandatory = $true)][string]$Topic,
  [Parameter(Mandatory = $true)][int]$Pr,
  [Parameter(Mandatory = $true)][string]$Branch,
  [Parameter(Mandatory = $true)][int]$Port
)
$ErrorActionPreference = "Stop"
Set-Location "C:\Rprojects\teal.modules.clinical"
$src = "$env:LOCALAPPDATA\Temp\cursor\screenshots\teal.modules.clinical\inst\pr_verification\${Topic}_browser.png"
$dst = "inst\pr_verification\${Topic}_browser.png"
if (-not (Test-Path -LiteralPath $src)) {
  throw "Screenshot not found: $src"
}
Copy-Item -Force -LiteralPath $src -Destination $dst
Get-NetTCPConnection -LocalPort $Port -ErrorAction SilentlyContinue | ForEach-Object {
  Stop-Process -Id $_.OwningProcess -Force -ErrorAction SilentlyContinue
}
if (Test-Path -LiteralPath ".Rbuildignore") {
  $rb = Get-Content -LiteralPath ".Rbuildignore" -Raw
  if ($rb -notmatch "pr_verification") {
    Add-Content -LiteralPath ".Rbuildignore" "`n^inst/pr_verification/"
  }
}
$toAdd = @(".Rbuildignore", $dst, "dev/run_example_in_browser.R", "dev/extract_rd_examples.R", "dev/picks_pr_append_body.ps1", "dev/pr_verify_start.ps1", "dev/pr_verify_finish.ps1")
foreach ($p in $toAdd) {
  if (Test-Path -LiteralPath $p) {
    git add -- "$p"
  }
}
git status -sb
git commit -m "chore(pr): browser verification screenshot for $Topic" 2>&1
if ($LASTEXITCODE -ne 0) {
  Write-Warning "Nothing to commit or commit failed"
}
git push origin HEAD 2>&1
& ".\dev\picks_pr_append_body.ps1" -Pr $Pr -Topic $Topic -Branch $Branch -CodeFile "_topic_code.txt"
