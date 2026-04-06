# Append documentation + screenshot section to a GitHub PR body (idempotent marker).
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
  Write-Warning "Example code contains triple backticks; PR section may need manual fix."
}

$old = gh pr view $Pr --repo $repo --json body --jq .body
if ($null -eq $old) { $old = "" }

if ($old -match '<!-- pr-docs-example-verification -->') {
  $old = $old -replace '(?s)\r?\n---\r?\n\r?\n<!-- pr-docs-example-verification -->.*$', ''
}

$sb = New-Object System.Text.StringBuilder
[void]$sb.AppendLine("")
[void]$sb.AppendLine("---")
[void]$sb.AppendLine("")
[void]$sb.AppendLine("<!-- pr-docs-example-verification -->")
[void]$sb.AppendLine("")
[void]$sb.AppendLine("## Documentation example & browser verification")
[void]$sb.AppendLine("")
[void]$sb.AppendLine(("Screenshot ~4s after the teal Shiny app finished loading in a local browser, using the @examples section in man/{0}.Rd." -f $Topic))
[void]$sb.AppendLine("")
[void]$sb.AppendLine("**Reproduce locally**")
[void]$sb.AppendLine("")
[void]$sb.AppendLine('```bash')
[void]$sb.AppendLine("Rscript dev/run_example_in_browser.R $Topic <port>")
[void]$sb.AppendLine('# then open http://127.0.0.1:<port>/')
[void]$sb.AppendLine('```')
[void]$sb.AppendLine("")
[void]$sb.AppendLine('**`@examples` (as in Rd)**')
[void]$sb.AppendLine("")
[void]$sb.AppendLine('```r')
[void]$sb.AppendLine($code)
[void]$sb.AppendLine('```')
[void]$sb.AppendLine("")
[void]$sb.AppendLine("![$Topic browser verification]($imgUrl)")
[void]$sb.AppendLine("")

$body = $old + $sb.ToString()
$tmp = Join-Path $PWD "_pr_body_tmp.md"
$utf8NoBom = New-Object System.Text.UTF8Encoding $false
[System.IO.File]::WriteAllText($tmp, $body, $utf8NoBom)
try {
  gh pr edit $Pr --repo $repo --body-file $tmp
} finally {
  Remove-Item -LiteralPath $tmp -Force -ErrorAction SilentlyContinue
}
