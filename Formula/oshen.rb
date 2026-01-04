class Oshen < Formula
  desc "A terminal shell that's just tryin' to be fun."
  homepage "https://github.com/lostintangent/oshen"
  version File.read(File.expand_path("../scripts/VERSION", __dir__)).strip
  license "MIT"

  on_macos do
    on_arm do
      url "https://github.com/lostintangent/oshen/releases/latest/download/oshen-macos-arm64.tar.gz"
      sha256 "6b14ebb4533eff55bc21d3aa5c19397887e3f019b028c1a2138e7847f29dfcc8"
    end
    on_intel do
      url "https://github.com/lostintangent/oshen/releases/latest/download/oshen-macos-x86_64.tar.gz"
      sha256 "44242dc976808ff566e2993d713871b551346f6b2b79d8616fd5bf68ae73a38c"
    end
  end

  on_linux do
    url "https://github.com/lostintangent/oshen/releases/latest/download/oshen-linux-x86_64.tar.gz"
    sha256 "4d0c754f4a0d26f650924478da8a228a29bb02aa540319f390788d5f684d1582"
  end

  def install
    bin.install "oshen"
  end

  test do
    assert_equal "hello", shell_output("#{bin}/oshen -c 'echo hello'").strip
  end
end
