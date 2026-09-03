import numpy as np
from scipy import stats
import pandas as pd

def _smooth(x):
    """Smooth the input array x using a median filter.
    
    This function replaces each element in the array with the median of itself and its neighbors,
    iteratively updating until no changes occur.
    
    Args:
        x (numpy.ndarray): Input array to be smoothed.

    Returns:
        numpy.ndarray: Smoothed array.
    """
    smooth = x.copy()  # Create a copy of the input array
    n_changes = 1  # Initialize a variable to track changes
    while n_changes != 0:
        prev = smooth.copy()  # Store the previous state
        for i in range(1, len(x)-1):
            # Update each element to the median of itself and its neighbors
            smooth[i] = np.median(prev[i-1:i+2])
        
        # Handle the first and last elements with edge cases
        smooth[0] = np.median([prev[0], smooth[1], 3 * smooth[1] - 2 * smooth[2]])
        smooth[-1] = np.median([prev[-1], smooth[-2], 3 * smooth[-2] - 2 * smooth[-3]])
        
        # Count the number of changes
        n_changes = np.sum(smooth != prev)
    return smooth


def _cummax(x):
    """Compute the cumulative maximum of the input array x.
    
    Args:
        x (numpy.ndarray): Input array.

    Returns:
        numpy.ndarray: Array where each element is the maximum value from the start up to that position.
    """
    y = np.array([np.max(x[:i]) for i in range(1, len(x)+1)])
    return y


def check(list):
    """Check if all elements in the list are identical.
    
    Args:
        list (list): Input list.

    Returns:
        bool: True if all elements are the same, False otherwise.
    """
    return len(set(list)) == 1

def rra(data,ties_method, prior=0.002, num_bin=800, num_iter=1000, return_all=True, corr_stop=1):
    """
    Robust Rank Aggregation Algorithm.
    
    This function aggregates rankings using a Bayesian approach, updating guesses iteratively.
    
    Args:
        data (numpy.ndarray): The rank data to be aggregated, shape (num_samples, num_features).
        prior (float): Prior probability for the Bayesian update.
        num_bin (int): Number of bins to categorize rank data.
        num_iter (int): Maximum number of iterations to run the algorithm.
        return_all (bool): If True, return all intermediate results; otherwise, return only the final guess.
        corr_stop (float): The correlation threshold for stopping the algorithm early.

    Returns:
        Depending on return_all:
            If True: tuple (guess, bayes_data, bayes_factors, converged, corr_last, corr_values)
            If False: tuple (guess, converged, corr_last, corr_values)
    """
    nr, nc = data.shape  # Get the number of rows (samples) and columns (features)
    nrp = int(np.floor(nr * prior))  # Calculate the number of top-ranked items to consider

    print(f'Nrp: {nrp}')  # Print the number of top ranks considered
    
    # Normalize rank data, with ranks reversed (higher ranks become lower)
    rank_data = np.array([stats.rankdata(-col,method=ties_method) / float(nr) for col in data.T]).T

    # Initialize arrays for Bayesian factors and binned data
    bayes_factors = np.zeros((num_bin, nc))
    binned_data = np.ceil(rank_data * num_bin).astype(int)
    bayes_data = np.zeros((nr, nc))

    guess = np.mean(rank_data, axis=1)  # Initial guess is the mean of the rank data
    cprev = 0  # Previous correlation
    corr_values = []  # Store correlation values for checking convergence
    converged = False  # Flag for convergence

    # Iteratively update guesses based on Bayesian factors
    for iter in range(num_iter):
        # Check for convergence based on correlation with the previous guess
        if corr_stop - cprev < 1e-15:
            print('Converged!')
            converged = True
            break
        elif iter >= 10 and check(corr_values[-10:]):
            converged = False  
            break

        guess_last = guess.copy()  # Store the last guess for correlation checking
        sorted_indices = np.argsort(guess)  # Get indices that would sort the guess
        guess[sorted_indices[:nrp]] = 1.0  # Assign top ranks to 1.0
        guess[sorted_indices[nrp:]] = 0.0  # Assign the rest to 0.0

        # Update Bayes factors for each feature
        for i in range(nc):
            for bin in range(1, num_bin + 1):
                tpr = np.sum(guess[binned_data[:, i] <= bin])  # True positive rate
                fpr = np.sum((1.0 - guess)[binned_data[:, i] <= bin])  # False positive rate
                bayes_factors[bin-1, i] = np.log((tpr + 1.0) / (fpr + 1.0) / (prior / (1.0 - prior)))

        # Smooth and enforce monotonic decrease of Bayes factors
        for i in range(nc):
            bayes_factors[:, i] = _smooth(bayes_factors[:, i])
            bayes_factors[:, i] = _cummax(bayes_factors[:, i][::-1])[::-1]

        # Update bayes_data with Bayes factors
        for i in range(nc):
            bayes_data[:, i] = bayes_factors[binned_data[:, i] - 1, i]

        # Update guess based on aggregated Bayes data
        guess = stats.rankdata(-np.sum(bayes_data, axis=1),method=ties_method)

        # Calculate correlation between the current and last guess
        cprev = stats.pearsonr(guess, guess_last)[0]
        print(f'Correlation with previous iteration: {cprev}')  # Print correlation value
        corr_values.append(cprev)
    
    corr_last = cprev  # Last correlation value
    
    if return_all:
        return guess, bayes_data, bayes_factors, converged, corr_last, corr_values
    else:
        return guess, converged, corr_last, corr_values


def CRank(file_name, sheet_name,ties_method, prior, num_bin, num_iter):
    """Read data from an Excel file and perform Robust Rank Aggregation.
    
    This function reads the ranking data from the specified sheet in an Excel file and calls the
    Robust Rank Aggregation algorithm on the data.

    Args:
        file_name (str): Path to the Excel file.
        sheet_name (str): Name of the sheet to read data from.
        prior (float): Prior probability for Bayesian updating.
        num_bin (int): Number of bins for categorizing ranks.
        num_iter (int): Maximum number of iterations for the RRA algorithm.

    Returns:
        tuple: Results of the RRA, including aggregated ranks and convergence information.
    """
    # Read data from the specified sheet in the Excel file
    df = pd.read_excel(file_name, sheet_name=sheet_name)
    df_edit = df.iloc[:, 1:].to_numpy()  # Extract data, ignoring the first column (assumed to be identifiers)

    # Call the RRA function and get results
    crank_res, converged, corr_last, corr_values = rra(df_edit, prior=prior, num_bin=num_bin, num_iter=num_iter, ties_method=ties_method, return_all=False, corr_stop=1)
    
    # Prepare the results in a DataFrame
    crank_res = pd.DataFrame(crank_res, columns=['CRank'])
    crank_res['Drug'] = df.iloc[:, 0]  # Add the drug identifiers from the first column
    crank_res = crank_res.iloc[:, [1, 0]]  # Reorder columns to have 'Drug' first

    return crank_res, converged, str(corr_last), corr_values
