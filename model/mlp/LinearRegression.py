# Betsy follow the MiT Licence.(c) xxl, All rights reserved
# Author xxl  Time:2024.7.29  Version:1.0
# simple linear regression unit
# MIT License
#
# Copyright (c) 2017
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import torch
import torch.nn as nn
import numpy as np
import matplotlib.pyplot as plt

# Hyper-parameters
input_size = 1
output_size = 1
num_epochs = 200
learning_rate = 0.001


def get_dataset():
    x_train = np.array([[3.3], [4.4], [5.5], [6.71], [6.93], [4.168],
                        [9.779], [6.182], [7.59], [2.167], [7.042],
                        [10.791], [5.313], [7.997], [3.1]], dtype=np.float32)

    y_train = np.array([[1.7], [2.76], [2.09], [3.19], [1.694], [1.573],
                        [3.366], [2.596], [2.53], [1.221], [2.827],
                        [3.465], [1.65], [2.904], [1.3]], dtype=np.float32)

    return x_train, y_train


def base_model():
    # simple linear regression model
    model = nn.Linear(input_size, output_size)
    return model


def train():
    x_train, y_train = get_dataset()
    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
    model = base_model().to(device)
    criterion = nn.MSELoss().to(device)
    optimizer = torch.optim.SGD(model.parameters(), lr=learning_rate)

    for epoch in range(num_epochs):
        # Convert numpy arrays to torch tensors
        inputs = torch.from_numpy(x_train).to(device)
        targets = torch.from_numpy(y_train).to(device)

        # Forward pass
        outputs = model(inputs)
        loss = criterion(outputs, targets)

        # Backward and optimize
        optimizer.zero_grad()
        loss.backward()  # calculate the gradient
        optimizer.step()  # update the parameters

        if (epoch + 1) % 5 == 0:
            print('Epoch [{}/{}], Loss: {:.4f}'.format(epoch + 1, num_epochs, loss.item()))

    torch.save(model.state_dict(), '../checkpoint/LinearRegression.pth')


def eval_linear_regression():
    weight = torch.load('../checkpoint/LinearRegression.pth')
    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
    model = base_model()
    model.load_state_dict(weight)
    model.to(device)
    x_train, y_train = get_dataset()
    y_test = []
    model.eval()
    with torch.no_grad():
        for x in x_train:
            x = torch.from_numpy(x).to(device)
            predicted = model(x).cpu().numpy()
            y_test.append(predicted)

    plt.plot(x_train, y_train, 'ro', label='Original data')
    plt.plot(x_train, y_test, label='Fitted line')
    plt.legend()
    plt.savefig("./LinearRegression.jpg")


if __name__ == '__main__':
    # train()

    # after train
    eval_linear_regression()
