--- a/tensorflow/workspace.bzl
+++ b/tensorflow/workspace.bzl
@@ -353,11 +353,11 @@ def tf_workspace(path_prefix="", tf_repo_name=""):
   tf_http_archive(
       name = "protobuf_archive",
       urls = [
-          "https://mirror.bazel.build/github.com/google/protobuf/archive/396336eb961b75f03b25824fe86cf6490fb75e3a.tar.gz",
-          "https://github.com/google/protobuf/archive/396336eb961b75f03b25824fe86cf6490fb75e3a.tar.gz",
+          "https://mirror.bazel.build/github.com/dtrebbien/protobuf/archive/50f552646ba1de79e07562b41f3999fe036b4fd0.tar.gz",
+          "https://github.com/dtrebbien/protobuf/archive/50f552646ba1de79e07562b41f3999fe036b4fd0.tar.gz",
       ],
-      sha256 = "846d907acf472ae233ec0882ef3a2d24edbbe834b80c305e867ac65a1f2c59e3",
-      strip_prefix = "protobuf-396336eb961b75f03b25824fe86cf6490fb75e3a",
+      sha256 = "eb16b33431b91fe8cee479575cee8de202f3626aaf00d9bf1783c6e62b4ffbc7",
+      strip_prefix = "protobuf-50f552646ba1de79e07562b41f3999fe036b4fd0",
   )
