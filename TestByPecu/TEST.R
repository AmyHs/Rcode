library(NLP)
library(readtext)
library(tm)
library(jiebaR)
library(jiebaRD)

# 1. get text data
#先取得老師上課範例的文本
page <- readtext("*.txt", encoding = "big5")
#因為等等用的tm的函式只吃Corpus類型的檔案所以把文字檔轉換
docs <- Corpus(VectorSource(page$text))
#為了等下寫迴圈時可以一次進行很多個文本，所以先知道有幾份文本
docnum = length(page)

# 2. tokenlized
#在迴圈以前先用jiebaR裡的辭庫等等拿來斷詞
mixseg = worker()
#兩個空的list來放回圈跑出來的結果
Alltoken = list()
Allfreq = list()
for( c in 1:docnum )
{
  #將文本裡的文字提出來用jiebaR中的segment函式來斷詞，然後結果存成list
  token= list(jiebaR::segment(docs[[c]]$content, mixseg))
  #因為不只一個文本，所以利用append函式讓每次迴圈跑完產生的新的詞彙list和舊的合併
  Alltoken = append(Alltoken, token)
  # 3. word frequency
  #用table計算token裡存好的詞彙頻率，再存成dataframe再存成list
  freq = list(as.data.frame(table(Alltoken[c])))
  #同上面存斷詞的方法
  #因為不只一個文本，所以透過append將每次迴圈跑完新產生的詞彙頻率的list和舊的合併
  Allfreq = append(Allfreq, freq)
}
# 4. Merge All frequency table (TermDocumentMatrix)
#因為TermDocumentMatrix在我的電腦上字詞跑出來會是亂碼，所以用merge寫出相同概念的功能
#自己寫的比較有彈性，可以依自己的需求合併data
#用迴圈可以存不只兩個文本，可以存很多個
AllTable = Allfreq[[1]]
for( m in 1:1 )
{
  #merge來合併兩個文本詞彙存成的dataframe
  AllTable = merge(AllTable, Allfreq[[m+1]], by="Var1")
}

