def inference(config):
  outpath = config.save_dir / f"{item['id']}.yaml"

  url = f"http://localhost:{config.vllm_port}/generate"
  num_samples = config.num_samples
  batch_size = config.batch_size

  samples = []
    for _ in tqdm(range(num_samples // batch_size), desc=f"Item {item['id']}"):
      body = {
            "prompt": prompt,
            "max_tokens": config.max_tokens,
            "n": batch_size,
            "temperature": config.temperature,
            "top_p": config.top_p,
            "stop": config.stop_strings,
            "logprobs": 1,
        }
      response = requests.post(url, json=body)

    out = {
        "prompt": prompt,
        "question": item["problem"],
        "samples": samples,
        "gt_answer": item["solution"],
      }


@pydra.main(GenerateScriptConfig)
def main(
    config: GenerateScriptConfig,
):

    test_dataset = list(
        load_dataset(
            "hendrycks/competition_math", "main", split="test", trust_remote_code=True
        )
    )
    train_dataset = list(
        load_dataset(
            "hendrycks/competition_math", "main", split="train", trust_remote_code=True
        )
    )

    print(f"Number of test items: {len(test_dataset)}")
    print(f"Number of train items: {len(train_dataset)}")

    random.seed(config.seed)

    for i, data in enumerate(train_dataset):
        data["id"] = i
