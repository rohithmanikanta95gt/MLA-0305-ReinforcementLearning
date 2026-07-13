# Install package (only once)
install.packages("DiagrammeR")

# Load library
library(DiagrammeR)

grViz("

digraph MDP_Process {

graph[
layout = dot,
rankdir = TB,
bgcolor = white,
labelloc = t,
fontsize = 24,
fontname = Arial,
label = 'MARKOV DECISION PROCESS WORKFLOW'
]

node[
shape = rectangle,
style = 'rounded,filled',
fontname = Arial,
fontsize = 14,
width = 4.5,
height = 0.9,
color = '#2E4053',
penwidth = 2
]

edge[
color = '#566573',
penwidth = 2,
arrowsize = 0.8
]

N1[
label='Input Data

• Patient records
• Symptoms
• Medical history
• Test reports',
fillcolor='#D6EAF8'
]

N2[
label='State Definition

• Identify health states
• Encode features
• Build state space
• Normalize data',
fillcolor='#D5F5E3'
]

N3[
label='Action Choice

• Select treatment
• Recommend medication
• Diagnosis
• Explore policy',
fillcolor='#FCF3CF'
]

N4[
label='State Update

• Predict next state
• Transition model
• Update condition
• Estimate dynamics',
fillcolor='#FADBD8'
]

N5[
label='Reward Calculation

• Recovery reward
• Risk penalty
• Reduce cost
• Improve outcome',
fillcolor='#F9E79F'
]

N6[
label='Policy Analysis

• Evaluate policy
• Compare actions
• Estimate value
• Improve decisions',
fillcolor='#E8DAEF'
]

N7[
label='Optimal Policy

• Best action
• Maximum reward
• Treatment strategy
• Final policy',
fillcolor='#F8C471'
]

N8[
label='Performance Check

• Accuracy
• Precision
• Recall
• F1-Score',
fillcolor='#ABEBC6'
]

N1 -> N2
N2 -> N3
N3 -> N4
N4 -> N5
N5 -> N6
N6 -> N7
N7 -> N8

subgraph cluster_mdp{

label='MDP Workflow'

fontsize=18
fontcolor=white
color='#1B4F72'
fillcolor='#F4F6F7'
style='rounded,filled'
penwidth=3

N1;N2;N3;N4;N5;N6;N7;N8;

}

}

")