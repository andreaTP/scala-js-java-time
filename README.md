# scalanative-java-time

`scalanative-java-time` is a BSD-licensed reimplementation of the `java.time` API
of JDK8 for Scala native. It enables this API in Scalanative projects.

## Usage

Simply add the following line to your sbt settings:

```scala
libraryDependencies += TBD
```

If you have a `crossProject`, the setting must be used only in the JS part:

```scala
lazy val myCross = crossProject
  ...
  .jsSettings(
    libraryDependencies += TBD
  )
```

**Requirement**: you must use a host JDK8 to *build* your project, i.e., to
launch sbt. `scalanative-java-time` does not work on earlier JDKs.

## Work in Progress / linking errors

This library is a work in progress.
There are still many classes and methods that have not been implemented yet.
If you use any of those, you will get linking errors.

Feel free to [contribute](./CONTRIBUTING.md) to extend the set of supported
classes and methods!

## Credits

This library is a direct port of [scalajs-java-time](https://github.com/scala-js/scala-js-java-time) for Scala.js to Scala native, special thanks to the original creators and contributors.

## License

`scalanative-java-time` is distributed under the
[BSD 3-Clause license](./LICENSE.txt).

## Contributing

Follow the [contributing guide](./CONTRIBUTING.md).
