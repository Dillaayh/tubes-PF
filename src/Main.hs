{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Data.Text as T
import Data.Aeson (FromJSON, object, (.=), withObject)
import Data.Aeson.Types (parseJSON, (.:))

-- Tipe data JSON sederhana
newtype InputText = InputText { input :: T.Text }
    deriving (Show)

instance FromJSON InputText where
    parseJSON = withObject "InputText" $ \v -> InputText <$> v .: "input"

-- Fungsi untuk menghitung jumlah kata
hitungKata :: T.Text -> Int
hitungKata teks = length (T.words teks)

-- Fungsi untuk menghitung jumlah kalimat
hitungKalimat :: T.Text -> Int
hitungKalimat teks = length . filter (not . T.null) $ T.splitOn "." teks

-- Fungsi untuk menghitung jumlah karakter
hitungKarakter :: T.Text -> Int
hitungKarakter teks = T.length teks

main :: IO ()
main = scotty 3000 $ do
    post "/hitung-kata" $ do
        inputData <- jsonData :: ActionM InputText
        let teks = input inputData
        let result = hitungKata teks
        json $ object ["result" .= result]

    post "/hitung-kalimat" $ do
        inputData <- jsonData :: ActionM InputText
        let teks = input inputData
        let result = hitungKalimat teks
        json $ object ["result" .= result]

    post "/hitung-karakter" $ do
        inputData <- jsonData :: ActionM InputText
        let teks = input inputData
        let result = hitungKarakter teks
        json $ object ["result" .= result]
