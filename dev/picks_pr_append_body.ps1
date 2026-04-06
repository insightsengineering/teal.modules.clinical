# Set GitHub PR body to example code + screenshot only (no headings or extra prose).
# Usage: .\dev\picks_pr_append_body.ps1 -Pr 1492 -Topic tm_t_glm_counts -Branch "tm_t_glm_counts@picks_modules_migration@279-interactive_variables@main" -CodeFile path\to\code.txt

param(
  [Parameter(Mandatory = $true)][int]$Pr,
  [Parameter(Mandatory = $true)][string]$Topic,
  [Parameter(Mandatory = $true)][string]$Branch,
  [Parameter(Mandatory = $true)][string]$CodeFile
)

$ErrorActionPreference = "Stop"
$repo = "insightsengineering/teal.modules.clinical"
$enc = $Branch -replace "@", "%40"
$imgUrl = "https://raw.githubusercontent.com/insightsengineering/teal.modules.clinical/$enc/inst/pr_verification/${Topic}_browser.png"

$code = (Get-Content -LiteralPath $CodeFile -Raw).TrimEnd()
if ($code -match '```') {
  Write-Warning "Example code contains triple backticks; escape or trim before PR update."
}

$sb = New-Object System.Text.StringBuilder
[void]$sb.AppendLine('```r')
[void]$sb.AppendLine($code)
[void]$sb.AppendLine('```')
[void]$sb.AppendLine("")
[void]$sb.AppendLine("![]($imgUrl)")
[void]$sb.AppendLine("")

$body = $sb.ToString()
$tmp = Join-Path $PWD "_pr_body_tmp.md"
$utf8NoBom = New-Object System.Text.UTF8Encoding $false
[System.IO.File]::WriteAllText($tmp, $body, $utf8NoBom)
try {
  gh pr edit $Pr --repo $repo --body-file $tmp
} finally {
  Remove-Item -LiteralPath $tmp -Force -ErrorAction SilentlyContinue
}
