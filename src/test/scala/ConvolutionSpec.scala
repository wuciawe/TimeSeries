import operations.Convolution
import org.scalatest.FreeSpec

class ConvolutionSpec extends FreeSpec {

  "A Convolution" - {
    "in full mode" - {
      "should work correctly in test#1" in {
        assert(Convolution.convolve(Vector(1, 2, 3), Vector(0, 1, 0.5), Convolution.Full) === Vector(0, 1, 2.5, 4, 1.5))
      }

      "shold work correctly in test#2" in {
        assert(Convolution.convolve(Vector(4, 3), Vector(1, 1, 5, 5), Convolution.Full) === Vector(4, 7, 23, 35, 15))
      }
    }

    "in same mode" - {
      "should work correctly in test#1" in {
        assert(Convolution.convolve(Vector(1, 2, 3), Vector(0, 1, 0.5), Convolution.Same) === Vector(1, 2.5, 4))
      }

      "shold work correctly in test#2" in {
        assert(Convolution.convolve(Vector(4, 3), Vector(1, 1, 5, 5), Convolution.Same) === Vector(4, 7, 23, 35))
      }
    }

    "in valid mode" - {
      "should work correctly in test#1" in {
        assert(Convolution.convolve(Vector(1, 2, 3), Vector(0, 1, 0.5), Convolution.Valid) === Vector(2.5))
      }

      "shold work correctly in test#2" in {
        assert(Convolution.convolve(Vector(4, 3), Vector(1, 1, 5, 5), Convolution.Valid) === Vector(7, 23, 35))
      }
    }
  }

}