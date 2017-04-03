{-# LANGUAGE OverloadedStrings #-}

import Test.WebDriver hiding (runWD)
import Test.Hspec.WebDriver

config = useBrowser chrome defaultConfig { wdHost = "selenium", wdPort = 4444}

allBrowsers :: [(Capabilities, String)]
allBrowsers = [(chromeCaps, "Chrome")]

main :: IO ()
main = hspec $
  describe "E2E smoke test" $ do

    sessionWith config "integration" $ using allBrowsers $ do
      it "checks all text in p" $ runWD $ do
        openPage "http://hhttpserver_run_for_test/test/files/index.html"
        e <- findElem $ ByCSS "p"
        e `shouldHaveText` ("Some HTML")

      it "checks all text in p strong" $ runWD $ do
        openPage "http://hhttpserver_run_for_test/test/files/index.html"
        e <- findElem $ ByCSS "p strong"
        e `shouldHaveText` ("HTML")
