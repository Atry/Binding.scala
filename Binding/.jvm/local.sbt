bloopInstall := {
  // override bloopInstall for debugging
  streams.value.log.info(
    libraryDependencies.value.toString
  )
  streams.value.log.info(
    (Compile / scalacOptions).value.toString
  )

  bloopInstall.value
}
