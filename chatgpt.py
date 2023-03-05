import openai
import os


openai.api_key = os.getenv("OPENAI_API_KEY")

def chatgpt(prompt: str | None = None):
    model = "gpt-3.5-turbo"

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
    chat = chatgpt()
    print(":: Ready")
    while True:
        try:
            reply = chat(input())
            if reply is not None:
                print(reply)
        except EOFError:
            break
