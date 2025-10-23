setup:
  pnpm install

build-typescript:
  tsc --project tsconfig.build.json

build-import-map:
  #!/usr/bin/env nu
  # Reference: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules#importing_modules_using_import_maps
  mkdir build
  glob src/**/*.ts 
    | path relative-to ("./src" | path expand) 
    | where not ($it =~ "\\.d\\.ts$") 
    | each { parse "{name}.ts" 
      | get 0 
      | { $"~/($in.name).js": $"/.js/($in.name).js" }
    } 
    | into record
    | save -f build/importmap.json
