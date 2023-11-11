import ParseSpec(term_props)
import Hedgehog (check)

main :: IO ()
main = mapM_ check term_props