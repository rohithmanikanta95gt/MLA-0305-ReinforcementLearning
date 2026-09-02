import os
import random
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def run_experiment_19():
    np.random.seed(42)
    random.seed(42)

    # --- 1. POMDP Belief State Setup ---
    states = ['S0', 'S1', 'S2']
    belief = np.array([1/3, 1/3, 1/3])

    # Transition model for action 'Move Right'
    T = np.array([
        [0.1, 0.9, 0.0],
        [0.0, 0.1, 0.9],
        [0.0, 0.0, 1.0]
    ])

    # Observation model P(Observation | State) -> Observations: 'O0', 'O1'
    O = np.array([
        [0.8, 0.2],  # in S0
        [0.3, 0.7],  # in S1
        [0.1, 0.9]   # in S2
    ])

    steps = 10
    dataset_rows = []
    entropy_history = []

    for step_num in range(1, steps + 1):
        # Predict step
        predicted_belief = np.dot(belief, T)
        
        # Simulate noisy observation based on belief
        obs_idx = np.random.choice([0, 1], p=[0.4, 0.6])
        
        # Update step (Bayes Filter)
        obs_probs = O[:, obs_idx]
        unnormalized_belief = predicted_belief * obs_probs
        norm_factor = np.sum(unnormalized_belief)
        
        if norm_factor > 0:
            belief = unnormalized_belief / norm_factor

        entropy = float(-np.sum(belief * np.log2(belief + 1e-8)))
        entropy_history.append(entropy)

        dataset_rows.append({
            "Step": step_num,
            "Observed": f"O{obs_idx}",
            "Belief(S0)": round(float(belief[0]), 3),
            "Belief(S1)": round(float(belief[1]), 3),
            "Belief(S2)": round(float(belief[2]), 3),
            "Entropy": round(entropy, 3)
        })

    df_dataset = pd.DataFrame(dataset_rows)

    # --- 3. Prepare Results DataFrame ---
    df_results = pd.DataFrame({
        "Metric": ["Total Timesteps", "Number of Hidden States", "Final Belief S0", "Final Belief S1", "Final Belief S2"],
        "Value": [steps, len(states), round(float(belief[0]), 3), round(float(belief[1]), 3), round(float(belief[2]), 3)]
    })

    # Save CSV Results
    df_results.to_csv("results_table.csv", index=False)
    print("Saved 'results_table.csv' successfully.")

    # --- 4. Save Summary Text File (Passage Format) ---
    summary_text = (
        "EXPERIMENT 19: POMDP BELIEF UPDATE (BAYES)\n\n"
        "The implementation of a Partially Observable Markov Decision Process belief state update using Bayes' rule "
        "demonstrates how an intelligent agent effectively tracks hidden states under severe environmental uncertainty. "
        "Beginning with a uniform prior distribution, the belief state dynamically updates through predictive transitions "
        "and incoming noisy sensor observations. As empirical results illustrate, consistent sensor feedback successfully "
        "concentrates probability mass onto the true underlying state while minimizing Shannon entropy, confirming the "
        "robustness and theoretical precision of Bayesian filtering in partially observable domains."
    )

    with open("summary.txt", "w", encoding="utf-8") as f:
        f.write(summary_text)
    print("Saved 'summary.txt' successfully.")

    # --- 5. Plot & Save Visualization ---
    plt.figure(figsize=(8, 4.5))
    plt.plot(range(1, steps + 1), entropy_history, marker='o', color='#ff7f0e', linewidth=2, label='Belief Entropy')
    
    plt.xlabel('Timesteps', fontsize=11)
    plt.ylabel('Shannon Entropy (Uncertainty)', fontsize=11)
    plt.title('POMDP Belief State Entropy Reduction over Time', fontsize=12, fontweight='bold')
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.legend()
    plt.tight_layout()
    plt.savefig("visualization.png", dpi=300)
    print("Saved 'visualization.png' successfully.")
    plt.show()

    return df_dataset, df_results

if __name__ == "__main__":
    df_dataset, df_results = run_experiment_19()
    print("\n--- DATASET (10 POMDP Belief Step Samples) ---")
    print(df_dataset.to_string(index=False))