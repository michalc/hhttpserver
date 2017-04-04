################################################################################
## integration test

variable "artifact_bucket" {
  type = "string"
  default = "artifacts.hhttpserver.charemza.name"
}

variable "account_id" {
  type = "string"
  default = "772663561820"
}

resource "aws_s3_bucket" "artifacts" {
  bucket = "${var.artifact_bucket}"
}

resource "aws_iam_role" "runner" {
  name = "hhttpserver_runner"
  assume_role_policy = "${data.aws_iam_policy_document.runner_assume_role.json}"
}

data "aws_iam_policy_document" "runner_assume_role" {
  statement {
    effect = "Allow"
    actions = [
      "sts:AssumeRole"
    ]
    principals {
      type = "Service"
      identifiers = ["codebuild.amazonaws.com"]
    }
    # condition {
    #   test = "StringLike"
    #   variable = "sts:ExternalId"
    #   values = [
    #     "arn:aws:codebuild:*:${var.account_id}:project/hhttpserver_*",
    #   ]
    # }
  }
}

resource "aws_iam_policy" "runner" {
  name = "hhttpserver_runner"
  policy = "${data.aws_iam_policy_document.runner.json}"
}

data "aws_iam_policy_document" "runner" {
  statement {
    effect = "Allow"
    actions = [
      "logs:CreateLogGroup",
      "logs:CreateLogStream",
      "logs:PutLogEvents"
    ]
    resources = [
      "*"
    ]
  }
  statement {
    effect = "Allow"
    actions = [
      "*"
    ]
    resources = [
      "arn:aws:s3:::${var.artifact_bucket}/*"
    ]
  }
}

resource "aws_iam_policy_attachment" "runner" {
  name = "hhttpserver_runner"
  policy_arn = "${aws_iam_policy.runner.arn}"
  roles = [
  	"${aws_iam_role.runner.id}"
  ]
}

resource "aws_codebuild_project" "build" {
  name = "hhttpserver_build"
  build_timeout = "5"
  service_role = "${aws_iam_role.runner.arn}"

  environment {
    compute_type = "BUILD_GENERAL1_SMALL"
    image = "aws/codebuild/docker:1.12.1"
    type = "LINUX_CONTAINER"
  }

  source {
    type = "CODEPIPELINE"
    buildspec = "${file("buildspec.build.yml")}"
  }

  artifacts {
    type = "CODEPIPELINE"
  }
}

resource "aws_codebuild_project" "test" {
  name = "hhttpserver_test"
  build_timeout = "5"
  service_role = "${aws_iam_role.runner.arn}"

  environment {
    compute_type = "BUILD_GENERAL1_SMALL"
    image = "aws/codebuild/docker:1.12.1"
    type = "LINUX_CONTAINER"
  }

  source {
    type = "CODEPIPELINE"
    buildspec = "${file("buildspec.test.yml")}"
  }

  artifacts {
    type = "CODEPIPELINE"
  }
}

resource "aws_iam_role" "master_pipeline" {
  name = "hhttpserver_master_pipeline"
  assume_role_policy = "${data.aws_iam_policy_document.master_pipeline_assume_role.json}"
}

data "aws_iam_policy_document" "master_pipeline_assume_role" {
  statement {
    effect = "Allow"
    actions = [
      "sts:AssumeRole"
    ]
    principals {
      type = "Service"
      identifiers = ["codepipeline.amazonaws.com"]
    }
    # condition {
    #   test = "StringLike"
    #   variable = "sts:ExternalId"
    #   values = [
    #     "arn:aws:codepipeline:*:${var.account_id}:hhttpserver_*",
    #   ]
    # }
  }
}

resource "aws_iam_role_policy" "master_pipeline" {
  name = "hhttpserver_master_pipeline"
  role = "${aws_iam_role.master_pipeline.id}"
  policy = "${data.aws_iam_policy_document.master_pipeline.json}"
}

data "aws_iam_policy_document" "master_pipeline" {
  # To get artifacts from one build to another
  statement {
    effect = "Allow"
    actions = [
      "*"
    ]
    resources = [
      # Might need GET on bucket versioning?
      "arn:aws:s3:::${var.artifact_bucket}",
    ]   
  }

  statement {
    effect = "Allow"
    actions = [
      "*"
    ]
    resources = [
      "arn:aws:s3:::${var.artifact_bucket}/*"
    ]   
  }

  statement {
    effect = "Allow"
    actions = [
      "codebuild:BatchGetBuilds",
      "codebuild:StartBuild"
    ]
    resources = [
      # Just limit to resource?
      "*"
    ]
  }
}

resource "aws_codepipeline" "master_pipeline" {
  name     = "hhttpserver_master_pipeline"
  role_arn = "${aws_iam_role.master_pipeline.arn}"

  artifact_store {
    location = "${var.artifact_bucket}"
    type     = "S3"
  }

  stage {
    name = "Source"

    action {
      name             = "Source"
      category         = "Source"
      owner            = "ThirdParty"
      provider         = "GitHub"
      version          = "1"
      output_artifacts = ["source"]

      configuration {
        Owner      = "michalc"
        Repo       = "hhttpserver"
        Branch     = "master"
      }
    }
  }

  stage {
    name = "Build"

    action {
      name            = "Build"
      category        = "Build"
      owner           = "AWS"
      provider        = "CodeBuild"
      input_artifacts = ["source"]
      output_artifacts = ["hhttpserver"]
      version         = "1"

      configuration {
        ProjectName = "hhttpserver_build"
      }
    }
  }

  stage {
    name = "Test"

    action {
      name            = "Test"
      category        = "Test"
      owner           = "AWS"
      provider        = "CodeBuild"
      input_artifacts = ["hhttpserver"]
      output_artifacts = []
      version         = "1"

      configuration {
        ProjectName = "hhttpserver_test"
      }
    }
  }
}
