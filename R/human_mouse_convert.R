#' inter-convert between human and mouse gene symbols based on user's need
#' 
#' This function allows you to convert a list of human genes to mouse gene symbols and vice versa
#' @param input_genes: the input gene list, must be a character vector
#' @param input_species: the input species that you want to convert from, could be "human" or "mouse". Must be in quotation marks. eg. if you put input_species = "human", then output species will be mouse
#' @return a vector of genes of the output species
#' @export
#' @examples
#' human_mouse_convert()
human_mouse_convert <- function(input_genes,input_species){
        if (input_species == "human"){
                require("biomaRt")
                human = useMart("ensembl", dataset = "hsapiens_gene_ensembl")
                mouse = useMart("ensembl", dataset = "mmusculus_gene_ensembl")
                genesV2 = getLDS(attributes = c("hgnc_symbol"), filters = "hgnc_symbol", values = input_genes , mart = human, attributesL = c("mgi_symbol"), martL = mouse, uniqueRows=T)
                mouse_out <- unique(genesV2[, 2])
                return(mouse_out)
        } else if (input_species == "mouse") {
                require("biomaRt")
                human = useMart("ensembl", dataset = "hsapiens_gene_ensembl")
                mouse = useMart("ensembl", dataset = "mmusculus_gene_ensembl")
                genesV2 = getLDS(attributes = c("mgi_symbol"), filters = "mgi_symbol", values = input_genes , mart = mouse, attributesL = c("hgnc_symbol"), martL = human, uniqueRows=T)
                humanx <- unique(genesV2[, 2])
                # Print the first 6 genes found to the screen
                print(head(humanx))
                return(humanx)
        }
}