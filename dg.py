#!/home/lepisma/.pyenv/shims/python

import os

from deepgram import (DeepgramClient, LiveOptions, LiveTranscriptionEvents,
                      Microphone)


def on_message(self, result, **kwargs):
    sentence = result.channel.alternatives[0].transcript

    if len(sentence) == 0:
        return

    print(f"Output: {result.to_json()}")

def on_metadata(self, metadata, **kwargs):
    print(f"\n\n{metadata}\n\n")

def on_speech_started(self, speech_started, **kwargs):
    print(f"\n\n{speech_started}\n\n")

def on_utterance_end(self, utterance_end, **kwargs):
    print(f"\n\n{utterance_end}\n\n")

def on_error(self, error, **kwargs):
    print(f"\n\n{error}\n\n")


def main():
    try:
        deepgram = DeepgramClient(
            os.getenv("DG_API_KEY")
        )

        dg_connection = deepgram.listen.live.v("1")

        dg_connection.on(LiveTranscriptionEvents.Transcript, on_message)
        dg_connection.on(LiveTranscriptionEvents.Metadata, on_metadata)
        dg_connection.on(LiveTranscriptionEvents.SpeechStarted, on_speech_started)
        dg_connection.on(LiveTranscriptionEvents.UtteranceEnd, on_utterance_end)
        dg_connection.on(LiveTranscriptionEvents.Error, on_error)

        options = LiveOptions(
            model="nova-2",
            punctuate=True,
            language="en-IN",
            encoding="linear16",
            channels=1,
            sample_rate=16000,
            # To get UtteranceEnd, the following must be set:
            interim_results=True,
            utterance_end_ms="1000",
            vad_events=True,
        )
        dg_connection.start(options)
        microphone = Microphone(dg_connection.send)

        microphone.start()
        input("Press Enter to stop recording...\n\n")
        microphone.finish()

        dg_connection.finish()

    except Exception as e:
        print(f"Could not open socket: {e}")
        return

if __name__ == "__main__":
    main()
