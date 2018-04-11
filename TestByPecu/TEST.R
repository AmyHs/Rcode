library(NLP)
library(readtext)
library(tm)
library(jiebaR)
library(jiebaRD)

# 1. get text data
#�����o�Ѯv�W�ҽd�Ҫ��奻
page <- readtext("*.txt", encoding = "big5")
#�]�������Ϊ�tm���禡�u�YCorpus�������ɮשҥH���r���ഫ
docs <- Corpus(VectorSource(page$text))
#���F���U�g�j��ɥi�H�@���i��ܦh�Ӥ奻�A�ҥH�����D���X���奻
docnum = length(page)

# 2. tokenlized
#�b�j��H�e����jiebaR�̪���w���������_��
mixseg = worker()
#��ӪŪ�list�ө�^��]�X�Ӫ����G
Alltoken = list()
Allfreq = list()
for( c in 1:docnum )
{
  #�N�奻�̪���r���X�ӥ�jiebaR����segment�禡���_���A�M�ᵲ�G�s��list
  token= list(jiebaR::segment(docs[[c]]$content, mixseg))
  #�]�����u�@�Ӥ奻�A�ҥH�Q��append�禡���C���j��]�����ͪ��s�����Jlist�M�ª��X��
  Alltoken = append(Alltoken, token)
  # 3. word frequency
  #��table�p��token�̦s�n�����J�W�v�A�A�s��dataframe�A�s��list
  freq = list(as.data.frame(table(Alltoken[c])))
  #�P�W���s�_������k
  #�]�����u�@�Ӥ奻�A�ҥH�z�Lappend�N�C���j��]���s���ͪ����J�W�v��list�M�ª��X��
  Allfreq = append(Allfreq, freq)
}
# 4. Merge All frequency table (TermDocumentMatrix)
#�]��TermDocumentMatrix�b�ڪ��q���W�r���]�X�ӷ|�O�ýX�A�ҥH��merge�g�X�ۦP�������\��
#�ۤv�g��������u�ʡA�i�H�̦ۤv���ݨD�X��data
#�ΰj��i�H�s���u��Ӥ奻�A�i�H�s�ܦh��
AllTable = Allfreq[[1]]
for( m in 1:1 )
{
  #merge�ӦX�֨�Ӥ奻���J�s����dataframe
  AllTable = merge(AllTable, Allfreq[[m+1]], by="Var1")
}
