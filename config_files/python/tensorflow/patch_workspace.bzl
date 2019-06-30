--- a/tensorflow/workspace.bzl
+++ b/tensorflow/workspace.bzl
@@ -120,11 +120,11 @@ def tf_workspace(path_prefix="", tf_repo_name=""):
   tf_http_archive(
       name = "eigen_archive",
       urls = [
-          "https://mirror.bazel.build/bitbucket.org/eigen/eigen/get/14e1418fcf12.tar.gz",
-          "https://bitbucket.org/eigen/eigen/get/14e1418fcf12.tar.gz",
+          "https://mirror.bazel.build/bitbucket.org/dtrebbien/eigen/get/374842a18727.tar.gz",
+          "https://bitbucket.org/dtrebbien/eigen/get/374842a18727.tar.gz",
       ],
-      sha256 = "2b526c6888639025323fd4f2600533c0f982d304ea48e4f1663e8066bd9f6368",
-      strip_prefix = "eigen-eigen-14e1418fcf12",
+      sha256 = "fa26e9b9ff3a2692b092d154685ec88d6cb84d4e1e895006541aff8603f15c16",
+      strip_prefix = "dtrebbien-eigen-374842a18727",
       build_file = str(Label("//third_party:eigen.BUILD")),
   )

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
