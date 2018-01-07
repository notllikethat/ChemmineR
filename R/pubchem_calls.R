
pubchemServerURL = "https://pubchem.ncbi.nlm.nih.gov/rest/pug"

pubchemCidToSDF = function(cids){

   if(! class(cids) == "numeric")
       stop('reference compound ids must be of class \"numeric\"')
   
   if(length(cids) == 0)
       stop('no compounds to retrieve- input must contain at least one cid')
   

	url = paste(pubchemServerURL,"compound","cid",paste(cids,collapse=","),"SDF",sep="/")
	read.SDFset(readLines(suppressWarnings(url(url))))
}
pubchemSmilesSearch = function(smiles){
	if(class(smiles) == "SMIset")
		smiles = as.character(smiles)
   if(! class(smiles) == "character"){
      stop('reference compound must be a smiles string of class \"character\"')
   } 
	url = paste(pubchemServerURL,"compound","fastsimilarity_2d","smiles",smiles,"SDF",sep="/")
	read.SDFset(readLines(suppressWarnings(url(url))))
}

pubchemSDFSearch = function(sdf){
   if(! class(sdf) == "SDFset"){
        stop('reference compound must be a compound of class \"SDFset\"')
   } 
   
	url = paste(pubchemServerURL,"compound","fastsimilarity_2d","sdf","SDF",sep="/")
	message("url: ",url)

	sdfStr = paste(as(sdf[[1]],"character"),collapse="\r\n")
	read.SDFset(readLines(textConnection(rawToChar(postForm(url,sdf = sdfStr)))))
}
pubchemSDF2PNG = function(sdf,outputFile){
	if(! class(sdf) == "SDFset"){
        stop('reference compound must be a compound of class \"SDFset\"')
   } 
   
	url = paste(pubchemServerURL,"compound","fastsimilarity_2d","sdf","PNG",sep="/")
	message("url: ",url)

	sdfStr = paste(as(sdf[[1]],"character"),collapse="\r\n")
	pngData = postForm(url,sdf=sdfStr)
	writeBin(pngData[1:length(pngData)],outputFile)
}
