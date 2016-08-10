
import Test.DocTest

main :: IO ()
main = doctest $
  "src/Servant/MatrixParam.hs" :
  "src/Servant/MatrixParam/Server.hs" :
  "-isrc" :
  "-XDataKinds" :
  "-XTypeOperators" :
  []
