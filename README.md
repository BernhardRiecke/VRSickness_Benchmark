
# VR Sickness Benchmark System

This VR sickness benchmark system is developed in Unity. It includes tools to induce and measure VR sickness in controlled scenarios. See http://ispace.iat.sfu.ca/project/vr-sickness-benchmark/ for additional documentation, videos, video tutorials about how to use and edit the Unity project, and related publications. Project-related videos can also be found on our YouTube playlist https://www.youtube.com/watch?v=SoXM-Z6kbvw&list=PLll6INaW-qaM6iZT6Sn5OxIVc8S2k8Ly_&pp=gAQBiAQB . 

## Getting Started

### Prerequisites

- SteamVR
- A VR headset (tested with HP Reverb G2 Omnicept and Oculus/Meta Quest)

### Installation

1. Clone or download the project from this repository.
2. Open the project in Unity (version 2020.3.39f1).
3. Ensure SteamVR is installed and running. Make sure your VR headset is properly connected and recognized by SteamVR.

### Setup

**Participant ID:** Before running the benchmark, you must manually set a participant ID. This ID is used to save and manage data for individual participants.

1. Navigate to `C:\Users\”yourUser”\AppData\LocalLow\H-BRS\Snowflakes`.
2. Create a file named `\participant_id.txt`.
3. Enter the participant's ID (e.g., 1) as the content of the file and save it.

**Key Bindings:** To ensure your key bindings are correct (up, down, and grip keys), you can either directly check this on SteamVR, or follow these steps:

1. On Unity, navigate to `Window > SteamVR Input Live View` to make sure “grabGrip”, “upperButton”, and “lowerButton” are set to your desired controller bindings. If not, proceed to the next step.
2. Navigate to `Window > SteamVR Input `> Scroll down and select `Open Binding UI` > Find grip, upper, and lower and set the bindings.

## Running the Benchmark

1. Open the `Scenes` folder within the Unity project.
2. You will find two main scenes: `Small Scene` and `Large Scene` to use. These correspond to the different environment sizes available for the benchmark. Select one to open.
3. Within the Unity Editor, locate the `Settings` GameObject in the scene hierarchy. Here, you can configure various aspects of the benchmark, including whether to enable/disable certain VR sickness mitigation techniques:
   - Static FOV reduction: vignette on
   - Dynamic FOV reduction: vignette on and AutoFOV
   - Blur: Dynamic Blur
   - Virtual Nose: Virtual Nose
4. To change the VR sickness threshold, change the “CS Abort Value”, it is set to 50 by default, indicating a moderate level of VR sickness.
5. Press the Play button in Unity to run the project.

## Controls

Once the project is running in VR:

- Press `C` to calibrate the headset position.
- Press `1` to start the pre-movement body sway measurement with eyes closed.
- Press `2` to start the pre-movement body sway measurement with eyes open.
- Hold the grip button (or space bar for keyboard fallback) for three seconds to initiate movement in the virtual environment.
- Use the `A` and `B` buttons on the VR controller to increase and decrease the VR sickness score, respectively, during movement.
- The movement will stop if participants reach a VR sickness score of 50% (or the abort value set).
- If you want to manually stop the movement, press `b`.
- After movement ceases, press `1` and `2` again for post-movement body sway measurements with eyes closed and open, respectively.

## Data Collection

Data collected during the experiment will be stored locally in the same folder as `participant_id.txt`. You should have three different files for each participant: 

- `sensoryData_participantID_environmentSize_Trial_#.csv`
- `SummaryData_participantID_environmentSize_Trial-#.csv`
- `participantID_Trial#.csv`

## Support

For support or to report issues, please email ber1@sfu.ca, rose_rouhani@sfu.ca, or jannik.brockerhoff@h-brs.de
see http://ispace.iat.sfu.ca/project/vr-sickness-benchmark/ for more details and videos of different stimuli
