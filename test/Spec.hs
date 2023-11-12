import ParseSpec(parse_props)
import Hedgehog (check)

main :: IO ()
main = mapM_ check parse_props