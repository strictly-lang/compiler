# strictly
## About
strictly is a declarative programming language for interactive web-uis.

## Goals
* Correctness
    * through type typesafety and immutability
    * through the declarative paradigm
* Safety
    * the compiler will protect you from xss
* Performance
    * no shadow dom diffing, all change paths are optimized at compile time
    * small memory footprint, therefore less garbage-collection freezes
* Size
    * there is no framework or runtime involved, everything is encapsulated inside the web component
* small scope
    * no feature-creep 
    * this language is aimed to solve dom interactions and state handling, not more not less

## Tradeoffs
* limited browser support
    * strictly is based on webcomponents
    * no walkarounds for outdated browser bugs
* no JS interop
    * there is no plan to make Javascript interoparability possible
    * the only interaction is through the webcomponent-api
- no ecosystem
    * no integration to js libraries
    * no integration to css libraries
- no feature richness
    * the scope of this language is very small and aims to solve only a limited scope

## State
This project is currently in its very early stages, the featureset and the apis will change.
