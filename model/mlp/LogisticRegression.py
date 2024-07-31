# Betsy follow the MiT Licence.(c) xxl, All rights reserved
# Author xxl  Time:2024.7.29  Version:1.0
# logistic regression unit
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
import torchvision
import torchvision.transforms as transforms
from torch.utils.data import DataLoader
import tqdm

# Hyper-parameters
input_size = 28 * 28  # 784
num_classes = 10
num_epochs = 50
batch_size = 128
learning_rate = 0.001


def get_dataset():
    # MNIST dataset (images and labels)
    train_dataset = torchvision.datasets.MNIST(root='../data',
                                               train=True,
                                               transform=transforms.ToTensor(),
                                               download=True)

    test_dataset = torchvision.datasets.MNIST(root='../data',
                                              train=False,
                                              transform=transforms.ToTensor())

    # Data loader (input pipeline)
    train_loader = DataLoader(dataset=train_dataset,
                              batch_size=batch_size,
                              shuffle=True)

    test_loader = DataLoader(dataset=test_dataset,
                             batch_size=batch_size,
                             shuffle=False)

    return train_loader, test_loader


def base_model():
    # Linear logistic
    model = nn.Linear(input_size, num_classes)
    return model


def train():
    train_loader, test_loader = get_dataset()
    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
    model = base_model().to(device)

    criterion = nn.CrossEntropyLoss().to(device)
    optimizer = torch.optim.SGD(model.parameters(), lr=learning_rate)

    total_step = len(train_loader)
    for epoch in tqdm.tqdm(range(num_epochs)):
        for i, (images, labels) in enumerate(train_loader):
            # Reshape images to (batch_size, input_size)
            images = images.reshape(-1, input_size).to(device)
            labels = labels.to(device)
            # Forward pass
            outputs = model(images)
            loss = criterion(outputs, labels)

            # Backward and optimize
            optimizer.zero_grad()
            loss.backward()
            optimizer.step()

            if (i + 1) % 100 == 0:
                print('Epoch [{}/{}], Step [{}/{}], Loss: {:.4f}'
                      .format(epoch + 1, num_epochs, i + 1, total_step, loss.item()))
    torch.save(model.state_dict(), '../checkpoint/LogisticRegression.pth')


def eval_logistic_regression():
    train_loader, test_loader = get_dataset()
    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
    model = base_model().to(device)
    weight = torch.load('../checkpoint/LogisticRegression.pth')
    model.load_state_dict(weight)

    model.eval()
    with torch.no_grad():
        correct = 0
        total = 0
        for images, labels in test_loader:
            labels = labels.to(device)
            images = images.reshape(-1, input_size).to(device)
            outputs = model(images)
            _, predicted = torch.max(outputs.data, 1)
            total += labels.size(0)
            correct += (predicted == labels).sum()

        print('Accuracy of the model on the 10000 test images: {} %'.format(100 * correct / total))


if __name__ == '__main__':
    # train()

    # after training the model (evaluate regression)
    eval_logistic_regression()