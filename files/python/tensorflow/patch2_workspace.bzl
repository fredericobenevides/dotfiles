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
