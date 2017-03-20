################################################################################
## integration test

resource "aws_iam_role" "integration_test" {
  name = "hhttpserver_integration_test"
  assume_role_policy = "${data.aws_iam_policy_document.integration_test_role.json}"
}

data "aws_iam_policy_document" "integration_test_role" {
  statement {
    effect = "Allow"
    actions = [
      "sts:AssumeRole"
    ]
    principals {
      type = "Service"
      identifiers = ["codebuild.amazonaws.com"]
    }
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

  artifacts {
    type = "NO_ARTIFACTS"
  }

  environment {
    compute_type = "BUILD_GENERAL1_SMALL"
    image        = "2"
    type         = "LINUX_CONTAINER"

    # environment_variable {
    #   "name"  = "SOME_KEY1"
    #   "value" = "SOME_VALUE1"
    # }

    # environment_variable {
    #   "name"  = "SOME_KEY2"
    #   "value" = "SOME_VALUE2"
    # }
  }

  source {
    type     = "GITHUB"
    location = "https://github.com/michalc/hhttpserver.git"
  }
}
