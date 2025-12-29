class Oshen < Formula
  desc "A terminal shell that's just tryin' to be fun."
  homepage "https://github.com/lostintangent/oshen"
  version File.read(File.expand_path("../scripts/VERSION", __dir__)).strip
  license "MIT"

  on_macos do
    on_arm do
      url "https://github.com/lostintangent/oshen/releases/latest/download/oshen-macos-arm64.tar.gz"
      sha256 "23a31ef048c65c1e4dd87d73230bd0bfee366304b9870acc621ba2a012f11fce"
    end
    on_intel do
      url "https://github.com/lostintangent/oshen/releases/latest/download/oshen-macos-x86_64.tar.gz"
      sha256 "0d26be929d5af667ca1947f762c593c93deec3c542025010123b792ac2551a7d"
    end
  end

  on_linux do
    url "https://github.com/lostintangent/oshen/releases/latest/download/oshen-linux-x86_64.tar.gz"
    sha256 "7533180ce0ec0043524d443e7873a1c90b9b810c880bea91c7291360b97432a1"
  end

  def install
    bin.install "oshen"
  end

  test do
    assert_equal "hello", shell_output("#{bin}/oshen -c 'echo hello'").strip
  end
end
