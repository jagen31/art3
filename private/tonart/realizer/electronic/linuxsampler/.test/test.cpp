#include<iostream>
#include<chrono>
#include<thread>
#include<filesystem>
#include<linuxsampler/Sampler.h>
#include<linuxsampler/drivers/audio/AudioOutputDeviceFactory.h>

namespace fs = std::__fs::filesystem;

int main() {
    auto sampler = new LinuxSampler::Sampler();
    auto factory = new LinuxSampler::AudioOutputDeviceFactory();

    auto params = std::map<std::string, std::string>();
    params["BUFFERSIZE"] = "2048";
    params["BUFFERS"] = "4";
    auto device = factory->Create("COREAUDIO", params);
auto organ = sampler->AddSamplerChannel();
organ->SetAudioOutputDevice(device);
organ->SetEngineType("SFZ");
organ->GetEngineChannel()->PrepareLoadInstrument(
(fs::current_path() / ".." / ".." / "resources" / "sfz" / "Jeux14" / "000/000_Montre_8.sfz").string().c_str(),
  0);
organ->GetEngineChannel()->LoadInstrument();

organ->GetEngineChannel()->SendNoteOn(53, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(750));
organ->GetEngineChannel()->SendNoteOff(53, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
organ->GetEngineChannel()->SendNoteOn(53, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
organ->GetEngineChannel()->SendNoteOff(53, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
organ->GetEngineChannel()->SendNoteOn(53, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
organ->GetEngineChannel()->SendNoteOff(53, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
organ->GetEngineChannel()->SendNoteOn(53, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(250));
organ->GetEngineChannel()->SendNoteOff(53, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
organ->GetEngineChannel()->SendNoteOn(52, 80, 0);
organ->GetEngineChannel()->SendNoteOn(56, 80, 0);
organ->GetEngineChannel()->SendNoteOn(59, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
organ->GetEngineChannel()->SendNoteOff(52, 80, 0);
organ->GetEngineChannel()->SendNoteOff(56, 80, 0);
organ->GetEngineChannel()->SendNoteOff(59, 80, 0);
organ->GetEngineChannel()->SendNoteOn(52, 80, 0);
organ->GetEngineChannel()->SendNoteOn(56, 80, 0);
organ->GetEngineChannel()->SendNoteOn(59, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(250));
organ->GetEngineChannel()->SendNoteOff(52, 80, 0);
organ->GetEngineChannel()->SendNoteOff(56, 80, 0);
organ->GetEngineChannel()->SendNoteOff(59, 80, 0);
organ->GetEngineChannel()->SendNoteOn(50, 80, 0);
organ->GetEngineChannel()->SendNoteOn(54, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(1250));
organ->GetEngineChannel()->SendNoteOff(50, 80, 0);
organ->GetEngineChannel()->SendNoteOff(54, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
}
