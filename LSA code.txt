##################################
#### Completed LSA in English ####
##################################
library(NLP)
library(tm)
library(SnowballC)
library(lsa)

### Import Corpus
#txt <- readLines("\\\\chss.datastore.ed.ac.uk/chss/ppls/users/s1537116/Win7/Desktop/Isabelle's/3.Python_code/BNC/2554(1).txt")
txt <- readLines("\\\\chss.datastore.ed.ac.uk/chss/ppls/users/s1537116/Win7/Desktop/Isabelle's/3.Python_code/BNC/2553.txt")
txt1<-strsplit(txt,". ", fixed=TRUE)
txt2<-gsub("[^.'0-9A-Za-z///' ]","" ,txt1)
txt3<-unlist(strsplit(txt2, ".", fixed = TRUE))


### Preprocessing
myCorpus <- Corpus(VectorSource(txt3))
#myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, stripWhitespace)
tm_map(myCorpus, stemDocument) #stemming

### Term-Document-Matrix
myTdm <- TermDocumentMatrix(myCorpus, list(dictionary=c('that', 'what', 'this', 'know', 'have', 'just', 'your', "it's", 'with', 'well', 'here', 'like', 'yeah', 'want', 'come', 'good', 'they', 'look', 'when', 'time', 'will', 'okay', 'back', 'mean', 'tell', "I'll", 'from', 'were', "he's", 'been', 'some', 'then', 'take', 'make', 'need', 'love', "I've", 'sure', 'them', 'more', 'over', 'down', 'very', 'said', 'much', 'life', 'even', 'give', 'only', 'help', 'talk', 'wait', 'into', 'find', 'call', 'told', 'ever', 'than', 'away', 'feel', 'work', 'fine', 'home', 'last', 'keep', 'does', 'stop', 'long', 'guys', 'kind', 'made', 'care', 'hear', 'baby', 'nice', 'else', 'stay', 'done', 'mind', 'hell', 'came', 'Jack', 'idea', 'best', 'must', 'room', 'left', 'knew', 'real', 'hope', 'name', 'same', 'went', 'girl', 'show', 'next', 'hurt', 'both', 'dead', 'kill', 'hard', 'hold', 'seen', 'deal', 'took', 'once', 'gone', 'head', 'most', 'used', 'part', 'live', 'face', 'TRUE', 'each', 'soon', 'wife', 'move', 'such', 'miss', 'meet', 'many', 'damn', 'lost', 'case', 'turn', 'wish', 'kids', 'late', 'five', 'town', 'year', 'play', 'hate', 'says', 'gave', 'fact', 'open', 'rest', 'word', 'glad', 'days', 'whoa', 'Mrs.', 'gets', 'door', 'hand', 'easy', 'walk', 'mine', 'shut', 'able', 'four', 'cool', 'eyes', 'news', 'half', 'side', 'read', 'pick', 'also', 'date', 'line', 'plan', 'lose', 'fire', 'free', 'shit', 'high', 'week', 'past', 'sick', 'game', 'goes', 'save', 'book', 'seem', 'sort', 'safe', 'shot', 'felt', 'Paul', 'full', 'dear', 'fuck', 'John', 'hour', 'lady', 'body', 'hang', 'send', 'died', 'Jake', 'sign', 'boys', 'hair', 'luck', 'Luis', 'kiss', 'pain', 'cold', 'fast', 'food', 'none', 'test', 'drop', 'sent', 'city', 'Lucy', 'poor', 'pull', 'busy', 'Joey', "we'd", 'less', 'step', 'fall', 'kept', 'till', "he'd", 'evil', 'Ross', 'trip', 'club', 'calm', 'Todd', 'fair', 'Rick', 'Rose', 'York', 'wake', 'nbsp', 'ring', 'dude', 'team', 'ride', 'wear', 'Nick', 'jail', 'Luke', 'tape', 'bill', 'feet', 'ones', 'list', 'mess', 'cute', 'Ryan', 'road', 'beat', 'fell', 'near', 'Beth', 'pass', 'gift', 'dark', 'self', 'Nora', 'aunt', 'sake', 'blow', 'mama', 'lied', 'Adam', 'Mike', 'ball', 'cops', 'blue', 'deep', 'park', 'Rafe', 'lord', 'join', 'card', 'soul', 'joke', 'cell', 'lead', 'paid', 'push', 'boss', 'risk', 'Eric', 'born', 'seat', 'song', 'boat', 'hide', 'nine', 'huge', 'sell', 'quit', 'Josh', 'Troy', 'fool', 'Mary', 'Alan', 'slow', 'kick', 'grab', 'rock', 'rich', 'arms', 'Brad', 'ways', 'cash', 'nose', 'Skye', 'beer', 'drug', 'Kyle', 'type', 'Alex', 'gosh', 'king', 'wall', 'jump', 'star', 'warm', 'suit', 'ruin', 'fish', 'fear', 'Anna', 'mail', 'cost', 'Liza', 'note', 'Chad', 'sing', 'foot', 'bank', 'Rory', 'tree', 'dare', 'shop', 'plus', 'roll', 'butt', 'nuts', 'Prue', 'grow', 'cake', 'mood', 'crap', 'lies', 'neck', 'land', 'lots', 'file', 'lock', 'wine', 'spot', 'Viki', 'fill', 'nope', 'keys', 'area', 'wild', 'Jill', 'desk', 'band', 'Neil', 'doin', 'sees', 'hall', 'wind', 'pack', 'ship', 'pool', 'held', 'jerk', 'copy', 'Keri', 'ahem', 'Tess', 'skin', 'code', 'page', 'burn', 'film', 'hero', 'dumb', 'whoo', 'hole', 'Hank', 'Reva', 'bear', 'bite', 'liar', 'Gwen', 'vote', 'gold', 'rule', 'upon', 'rush', 'blew', 'Kate', 'form', 'onto', "it'd", 'holy', 'duty', 'legs', 'Dave', 'loud', 'Andy', 'Jess', 'army', 'ugly', 'Seth', 'coat', 'moon', 'lips', 'size', 'Sami', 'sold', 'Lily', 'Cole', 'roof', 'bird', 'fake', 'bomb', 'Toby', 'clue', 'hook', 'port', 'dean', 'loss', 'view', 'heat', 'feed', 'drag', 'hire', 'warn', 'post', 'milk', 'suck', 'ends', 'dump', 'rent', 'rain', 'pray', 'meat', 'Raul', 'ours', 'cook', 'babe', 'main', 'jury', 'Stan', 'sexy', 'blah', 'cars', 'Pete', 'weak', 'trey', 'slip', 'Abby', 'Sean', 'dick', 'race', 'lake', 'deny', 'tear', 'ears', 'tiny', 'draw', 'harm', 'snow', 'dogs', 'base', 'lift', 'woke', 'lisa', 'camp', 'wise', 'doll', 'pair', 'edge', 'guns', 'bike', 'bone', 'term', 'meal', 'bail', 'hung', 'dawn', 'wash', 'lets', 'whom', 'tied', 'dies', 'rude', 'grew', 'soft', 'cast', 'fate', 'prom', 'east', 'unit', 'crew', 'guts', 'shoe', 'Kane', 'tall', 'runs', 'jeez', 'soup', 'chef', 'tory', 'Zack', 'male', 'tour', 'puts', 'whip', 'heck', 'laid', 'bust', 'pure', 'nail', 'hill', 'lane', 'dirt', 'bond')))

#colnames(myTdm)
#rownames(myTdm)
#dim(myTdm)

Tdm.matrix<-as.matrix(myTdm)
#dim(Tdm.matrix)

ptdm <- Tdm.matrix/rowSums(Tdm.matrix)
#rowSums(Tdm.matrix)
#dim(ptdm)

### Adding entropy
GW <-1-entropy(ptdm)/log(nrow(Tdm.matrix))
LW <-log(1 + Tdm.matrix)
x <- GW * LW
#x

### Singular Vector Decomposition
myLSA <- lsa(x)
#myLSA <- lsa(myTdm)

termVec<-myLSA$tk*myLSA$sk
#docVec<-myLSA$dk*myLSA$sk

termSimLSA<-cosine(t(termVec))
#termSimLSA

write.csv(termSimLSA, file="1.4orth_lsa.csv")
