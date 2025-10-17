# Use Lamma to name the topics 
# I recommend running this locally becuase Computing cluster requires special use code for attaching OLLAMA. 

library(rollama)
func_label_topic = function(vec_abs){
    vec_out = c()
    sys_prompt = paste0(
    "You’re an expert in finding common themes from a set of documents. ", 
    "Your job is to generate a phrase to describe a theme based the documents provided.",
    "These documents are all abstracts from psychological research papers that are highly relevant to a specific topic identified through structural topic modeling (STM). ",
    "Read all the documents carefully and then generate one concise phrase that accurately captures the essence of the topic. ",
    "The output should be the single phrase you generated, without any additional text or explanation."
    )
    for (i in 1:length(vec_abs)){
    rep_abs = vec_abs[i]
    print(paste0("Processing topic ", i, "; Number of representative abstracts: ", str_count(rep_abs, "\n\n---\n\n") + 1))
    query = make_query(
        template = "{prefix}\n{text}\n{suffix}", 
        system = sys_prompt,
        prefix = "Below are the documents, each separated by '\n\n---\n\n': ",
        text = rep_abs,
        suffix = "Do not give me a list of phrases; just provide one phrase that best represents the common theme across all the documents. Limit the phrase to three words."
    )
    model_params = list("temperature"=0, # Set temp to 0 to minimize the scores of low-probability tokens (AKA being anti-creativity)
                        "seed" = 1111) # set seed for reproducibility
    result = query(query, model_params = model_params, model = "llama3.2")
    out = result[[1]][["message"]][["content"]]
    vec_out = c(vec_out, out)
    }
    return(vec_out)
}

# Note: We can't provide the full set of abstracts here due to legal concerns about sharing copyrighted material.