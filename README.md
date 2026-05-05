# Scalafix rules

To develop rule:
```
sbt ~tests/test
# edit rules/src/main/scala/fix/ScalafixRules.scala
```

## Publish

> sbt publishLocal

## Usage example

> sbt scalafix dependency:ImplicitToUsing@uk.tumakha::scalafix-rules:0.1.0-SNAPSHOT
