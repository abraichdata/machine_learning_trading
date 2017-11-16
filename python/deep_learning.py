# -*- coding: utf-8 -*-
"""
Created on Thu Nov 16 16:45:49 2017

@author: Schliebs
"""

import numpy as np
input_data = np.array([2,3])
weights = {'node_0': np.array([1,1]),
           'node_1': np.array([-1,1]),
           'output': np.array([2,-1])
           }

node_0_value = (input_data*weights['node_0']).sum()
node_1_value = (input_data*weights['node_1']).sum()

hidden_layer_values = np.array([node_0_value,node_1_value])
print(hidden_layer_values)
output = (hidden_layer_values * weights ['output']).sum()
print(output)

"""
After this: use ReLu: Rectified linear activation function
"""

def relu(input):
    '''Define your relu activation function here'''
    # Calculate the value for the output of the relu function: output
    output = max(input, 0)
    
    # Return the value just calculated
    return(output)

# Calculate node 0 value: node_0_output
node_0_input = (input_data * weights['node_0']).sum()
node_0_output = relu(node_0_input)

# Calculate node 1 value: node_1_output
node_1_input = (input_data * weights['node_1']).sum()
node_1_output = relu(node_1_input)

# Put node values into array: hidden_layer_outputs
hidden_layer_outputs = np.array([node_0_output, node_1_output])

# Calculate model output (do not apply relu)
model_output = (hidden_layer_outputs * weights['output']).sum()

# Print model output
print(model_output)

