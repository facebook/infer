# Infer annotations

This project provides extra annotations for Java checkers, such as starvation.

## How to build JARs (suitable for release)

Make sure you have maven installed. `infer-annotaions` is a regular
maven project, so regular maven commands apply. The following commands might be of interest:

1. Build artifact jar (the one with class files): `mvn package`.
2. Build sources jar: `mvn source:jar`
3. Clean: mvn clean

The resulting artifacts will be under `target/` folder.
