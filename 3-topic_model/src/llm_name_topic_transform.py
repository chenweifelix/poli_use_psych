import json
import os
from typing import List, Dict, Any
from pathlib import Path
import pandas as pd
import re
from dotenv import load_dotenv
import requests
import time

load_dotenv()  # Load environment variables from .env file
api_key = os.getenv("OPENROUTER_API_KEY")
if not api_key:
    raise ValueError("OPENROUTER_API_KEY is not set.")

TEMPERATURE = 0
SEED = 1234
SERVER = "openrouter"
MODEL = "google/gemma-4-31b-it"

def get_llm_response_each(input_text: str, model: str, sys_prompt=None, max_retries=5, timeout=180):

    for attempt in range(max_retries):
        try:
            response = requests.post(
                url="https://openrouter.ai/api/v1/chat/completions",
                headers={
                    "Authorization": f"Bearer {os.getenv('OPENROUTER_API_KEY')}",
                    "Content-Type": "application/json"
                },
                json={
                    "model": MODEL,
                    "messages": [
                        {"role": "system", "content": sys_prompt},
                        {"role": "user", "content": input_text}
                    ],
                    "temperature": TEMPERATURE,
                    "seed": SEED
                },
                timeout=timeout
            )

            if response.status_code == 200:
                return {
                    "ok": True,
                    "response": response,
                    "attempt_count": attempt + 1,
                    "error_message": None
                }

            last_error = f"API error {response.status_code}: {response.text}"
            print(last_error)

            if attempt < max_retries - 1:
                wait_time = 2 ** attempt
                time.sleep(wait_time)
                continue

        except requests.exceptions.RequestException as e:
            last_error = f"Request failed: {e}"
            print(last_error)
            if attempt < max_retries - 1:
                wait_time = 2 ** attempt
                time.sleep(wait_time)
                continue

    return {
        "ok": False,
        "response": None,
        "attempt_count": max_retries,
        "error_message": last_error
    }



def summarize_abstracts(vec_abs: List[str], sys_prompt: str) -> List[str]:
    summaries = []
    for i, abs_text in enumerate(vec_abs):
        print(f"Summarizing abstract {i+1}/{len(vec_abs)}")
        if SERVER == "openrouter":
            summary = get_llm_response_each(abs_text, model=MODEL, sys_prompt=sys_prompt)
            summary_text = summary["response"].json()["choices"][0]["message"]["content"]
            summaries.append(summary_text)       
        else:
            summary = get_llm_response_each(sys_prompt, abs_text)
            summaries.append(summary)

    return summaries

def organize_summaries(summaries: List[str]) -> str:
    str_abs_input = ""
    for i, summ in enumerate(summaries):
        str_abs_input += f"Abstract summary {i+1}:\n{summ}\n\n"
    return str_abs_input


def name_topic_based_on_summaries(
            vec_abs: List[str], 
            summarize_prompt_path: str,
            name_prompt_path: str):
    
    with open(summarize_prompt_path, "r") as f:
        summarize_prompt = f.read()
    with open(name_prompt_path, "r") as f:
        sys_prompt_name = f.read()
        
    print("Summarizing abstracts...")
    summaries = summarize_abstracts(vec_abs, summarize_prompt)
    print("Organizing summaries for topic naming...")
    str_abs_input = organize_summaries(summaries)
    print("Naming topic based on summaries...")
    if SERVER == "openrouter":
        topic_name_resopnse = get_llm_response_each(str_abs_input, model=MODEL, sys_prompt=sys_prompt_name)
        topic_name = topic_name_resopnse["response"].json()["choices"][0]["message"]["content"]
    else:
        topic_name = get_llm_response_each(sys_prompt_name, str_abs_input)
    return summaries, topic_name

def parse_possible_json(text: str) -> dict:
    text = text.strip()
    text = re.sub(r"^```json\s*", "", text)
    text = re.sub(r"\s*```$", "", text)

    try:
        return json.loads(text)
    except json.JSONDecodeError:
        match = re.search(r"\{.*\}", text, flags=re.DOTALL)
        if match:
            return json.loads(match.group(0))
        raise

def name_one_topic_based_on_abs(
        topic_label : int,
        df_rep_abs_summ: pd.DataFrame,  # Representative abstracts for the topic, with a column named "abstract"
        summarize_prompt_path: str, 
        name_prompt_path: str,
        summary_out_path: str, 
        topic_name_desc_out_path: str):
    summary_out_filename = summary_out_path / f"topic_{topic_label}_summaries.csv"
    if summary_out_filename.exists():
        print(f"Skipping existing topic : {topic_label}")
        return None 
    else:
        vec_abs = (
            df_rep_abs_summ["abstract"]
            .dropna()
            .astype(str)
            .tolist()
            )
        topic_label = str(topic_label)
        topic_abstract_summaries, topic_name_desc = name_topic_based_on_summaries(
            vec_abs = vec_abs,
            summarize_prompt_path = summarize_prompt_path,
            name_prompt_path = name_prompt_path
        )
        df_rep_abs_summ = df_rep_abs_summ.copy()
        df_rep_abs_summ["summary"] = topic_abstract_summaries
        print(f"Saving summaries to {summary_out_filename}")
        df_rep_abs_summ.to_csv(summary_out_filename, index=False)

        try:
            response_json = parse_possible_json(topic_name_desc) # Check if the response is valid JSON
            topic_name_desc_filename = topic_name_desc_out_path / f"topic_{topic_label}.json"
            print(f"Saving topic name and description to {topic_name_desc_filename}")
            with open(topic_name_desc_filename, "w") as f:
                json.dump(response_json, f, indent=4)
        except:
            print("Can't parse topic name response as JSON. Saving the raw response instead.")
            topic_name_desc_filename = topic_name_desc_out_path / f"topic_{topic_label}_raw.txt"
            with open(topic_name_desc_filename, "w") as f:
                f.write(topic_name_desc)

        return df_rep_abs_summ, topic_name_desc

def loop_through_topics_and_name_them(
        list_abs_files: List[Path], 
        summarize_prompt_path: Path, 
        name_prompt_path: Path, 
        summary_out_path: Path, 
        topic_name_desc_out_path: Path):

    summary_out_path.mkdir(parents=True, exist_ok=True)
    topic_name_desc_out_path.mkdir(parents=True, exist_ok=True)
    for i, abs_file in enumerate(list_abs_files):
        print(f"Processing {abs_file.stem}; {i + 1} / {len(list_abs_files)}")
        # abs_file = list_abs_files[0]
        topic_label = abs_file.stem.split(" ")[-1] # Assuming the file name format is "Topic {number}.csv"
        df_topic = pd.read_csv(abs_file)
        name_one_topic_based_on_abs(
            topic_label = topic_label,
            df_rep_abs_summ = df_topic, # For testing, we can start with just the top 2 abstracts. Remove for the full run.
            summarize_prompt_path = summarize_prompt_path,
            name_prompt_path = name_prompt_path,
            summary_out_path = summary_out_path, 
            topic_name_desc_out_path = topic_name_desc_out_path)
    
    return None 

