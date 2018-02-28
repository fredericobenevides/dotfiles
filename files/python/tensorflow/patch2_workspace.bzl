--- a/tensorflow/workspace.bzl
+++ b/tensorflow/workspace.bzl
@@ -120,11 +120,11 @@ def tf_workspace(path_prefix="", tf_repo_name=""):
   tf_http_archive(
       name = "eigen_archive",
       urls = [
-          "https://mirror.bazel.build/bitbucket.org/eigen/eigen/get/2355b229ea4c.tar.gz",
-          "https://bitbucket.org/eigen/eigen/get/2355b229ea4c.tar.gz",
+          "https://mirror.bazel.build/bitbucket.org/dtrebbien/eigen/get/374842a18727.tar.gz",
+          "https://bitbucket.org/dtrebbien/eigen/get/374842a18727.tar.gz",
       ],
-      sha256 = "0cadb31a35b514bf2dfd6b5d38205da94ef326ec6908fc3fd7c269948467214f",
-      strip_prefix = "eigen-eigen-2355b229ea4c",
+      sha256 = "fa26e9b9ff3a2692b092d154685ec88d6cb84d4e1e895006541aff8603f15c16",
+      strip_prefix = "dtrebbien-eigen-374842a18727",
       build_file = str(Label("//third_party:eigen.BUILD")),
   )
