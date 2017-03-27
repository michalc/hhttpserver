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

resource "aws_iam_role" "integration_test" {
  name = "hhttpserver_integration_test"
  assume_role_policy = "${data.aws_iam_policy_document.integration_test_assume_role.json}"
}

data "aws_iam_policy_document" "integration_test_assume_role" {
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

resource "aws_iam_policy" "integration_test" {
  name = "hhttpserver_integration_test"
  policy = "${data.aws_iam_policy_document.integration_test.json}"
}

data "aws_iam_policy_document" "integration_test" {
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

resource "aws_iam_policy_attachment" "integration_test" {
  name = "hhttpserver_integration_test"
  policy_arn = "${aws_iam_policy.integration_test.arn}"
  roles = [
  	"${aws_iam_role.integration_test.id}"
  ]
}

resource "aws_codebuild_project" "integration_test" {
  name = "hhttpserver_integration_test"
  build_timeout = "5"
  service_role = "${aws_iam_role.integration_test.arn}"

  environment {
    compute_type = "BUILD_GENERAL1_SMALL"
    image = "aws/codebuild/docker:1.12.1"
    type = "LINUX_CONTAINER"
  }

  source {
    type = "CODEPIPELINE"
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
        ProjectName = "hhttpserver_integration_test"
      }
    }
  }
}
