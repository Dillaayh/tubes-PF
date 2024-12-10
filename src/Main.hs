{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Data.Text as T
import Data.Aeson (FromJSON, object, (.=), withObject)
import Data.Aeson.Types (parseJSON, (.:))
import Data.List (group, sort)
import qualified Data.Map as M

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

-- Fungsi untuk menghitung frekuensi kata
hitungFrekuensi :: T.Text -> [(T.Text, Int)]
hitungFrekuensi teks = 
    let wordsList = T.words teks
        countMap = M.fromListWith (+) [(word, 1) | word <- wordsList]
    in M.toList countMap

-- Fungsi untuk menghitung huruf kecil
hitungHurufKecil :: T.Text -> Int
hitungHurufKecil teks = length . filter (`elem` ['a'..'z']) $ T.unpack teks

-- Fungsi untuk menghitung huruf besar
hitungHurufBesar :: T.Text -> Int
hitungHurufBesar teks = length . filter isUpper $ T.unpack teks
  where
    isUpper c = c `elem` ['A'..'Z']


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

    -- Menambahkan endpoint untuk menghitung frekuensi kata
    post "/hitung-frekuensi" $ do
        inputData <- jsonData :: ActionM InputText
        let teks = input inputData
        let result = hitungFrekuensi teks
        json $ object ["result" .= result]

    -- Endpoint untuk menghitung huruf kecil
    post "/hitung-huruf-kecil" $ do
        inputData <- jsonData :: ActionM InputText
        let teks = input inputData
        let result = hitungHurufKecil teks
        json $ object ["result" .= result]

    -- Endpoint untuk menghitung huruf besar
    post "/hitung-huruf-besar" $ do
        inputData <- jsonData :: ActionM InputText
        let teks = input inputData
        let result = hitungHurufBesar teks
        json $ object ["result" .= result]
