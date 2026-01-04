class Oshen < Formula
  desc "A terminal shell that's just tryin' to be fun."
  homepage "https://github.com/lostintangent/oshen"
  version File.read(File.expand_path("../scripts/VERSION", __dir__)).strip
  license "MIT"

  on_macos do
    on_arm do
      url "https://github.com/lostintangent/oshen/releases/latest/download/oshen-macos-arm64.tar.gz"
      sha256 "d571e94713b73ca6a38729f10a72e23aa9f6b6946c32e9f96572abe12544ab58"
    end
    on_intel do
      url "https://github.com/lostintangent/oshen/releases/latest/download/oshen-macos-x86_64.tar.gz"
      sha256 "050f13b010fc1614dcc7c8d35adbcbeec9d82f2589adb03ca92bdcf4211d142f"
    end
  end

  on_linux do
    url "https://github.com/lostintangent/oshen/releases/latest/download/oshen-linux-x86_64.tar.gz"
    sha256 "4b384b7717c939ac1e64464f7f5aa375c08688e9132cf462b347d78b25d94cc4"
  end

  def install
    bin.install "oshen"
  end

  test do
    assert_equal "hello", shell_output("#{bin}/oshen -c 'echo hello'").strip
  end
end
