import operations.Convolution
import org.scalatest.FlatSpec

class ConvolutionSpec extends FlatSpec {
  it should "produce correct convolution result in full mode" in {
    assert(Convolution.convolve(Vector(1,2,3), Vector(0, 1, 0.5), Convolution.Full) === Vector(0, 1, 2.5, 4, 1.5))
  }

  it should "produce correct convolution result in valid mode" in {
    assert(Convolution.convolve(Vector(1,2,3), Vector(0, 1, 0.5), Convolution.Valid) === Vector(2.5))
  }
}