$ErrorActionPreference = "Stop"

$RepoRoot = (Resolve-Path (Join-Path $PSScriptRoot "..\..")).Path
$Platform = "linux/amd64"

docker buildx build `
  --load `
  --platform $Platform `
  --build-arg "BUILD_VIGNETTES=true" `
  --file (Join-Path $RepoRoot "docker\linux\Dockerfile") `
  --tag "drugsignet:linux" `
  $RepoRoot

if ($LASTEXITCODE -ne 0) {
  throw "Failed to build the DrugSigNet Linux base image."
}

docker buildx build `
  --load `
  --platform $Platform `
  --file (Join-Path $RepoRoot "docker\windows\Dockerfile") `
  --tag "drugsignet:windows" `
  $RepoRoot

if ($LASTEXITCODE -ne 0) {
  throw "Failed to build the DrugSigNet Windows Docker Desktop image."
}
