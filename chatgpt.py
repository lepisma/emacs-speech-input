#!/usr/bin/env python

import openai
import os


def chatgpt(prompt: str | None = None):
    """
    Context-less ChatGPT executor.
    """

    model = "gpt-4"

    def _fn(input_str: str) -> str | None:
        response = openai.ChatCompletion.create(
            model=model,
            messages=[
                {"role": "system", "content": prompt or ""},
                {"role": "user", "content": input_str}
            ],
            temperature=0
        )

        try:
            return response.choices[0]["message"].content
        except Exception:
            return None

    return _fn

if __name__ == "__main__":

    api_key = os.getenv("OPENAI_API_KEY")

    if api_key is None:
        raise ValueError("OPENAI_API_KEY is not set")

    openai.api_key = api_key

    chat = chatgpt()
    while True:
        try:
            reply = chat(input())
            if reply is not None:
                print(reply)
        except EOFError:
            break
