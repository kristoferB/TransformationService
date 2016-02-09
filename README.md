# Patient Diff Service

This project gives you a base template for creating transformation services. In this case it is a service that
listens to Elvis patient snapshots

## Quickstart

setup the correct configurations in the config file under src/main/resources

```
sbt
```

To enter sbt


## Starting the service

```
sbt run
```

## Packaging the service

See the SBT assembly plugin for more info. To package as zip file run

```
sbt universal:packageBin
```

## Running tests

```
sbt clean test
```
