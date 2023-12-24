
import torch
import torchvision
import torchvision.transforms as transforms
from torchvision import datasets
from torchvision.transforms import ToTensor
from neural.CNN.LeNet.Train import load_data_fashion_mnist,LeNet

'''
using the LeNet backbone to inference it
training about 200 iterations and the test accuracy is around 89% 

(1) get the pic and convert it to the sint 16

(2) get all weights to the sint 16 binary

'''

def inference(model,file,data,label):
    model.load_state_dict(torch.load(file))
    classes = [ "T-shirt/top", "Trouser", "Pullover", "Dress", "Coat", "Sandal",
                "Shirt", "Sneaker", "Bag", "Ankle boot"]
    model.eval()
    with torch.no_grad():
        pred = model(data)
        predicted,actual = classes[pred[0].argmax(0)], classes[label]
        print(f'Predicted: "{predicted}", Actual: "{actual}"')


def raw_data(download = False):
    training_data = datasets.FashionMNIST(root="Datasets/FashionMNIST",train=True,download=download,transform=ToTensor())
    test_data = datasets.FashionMNIST(root="Datasets/FashionMNIST",train=False,download=download,transform=ToTensor())
    return training_data,test_data


def Weight2Bin(file):
    '''
    :param file: load the weight file and trans it into SINT 16
    :return:
    '''
    model_dict = torch.load(file)
    fp = open('model_parameter.bin', 'wb')
    weight_count = 0
    num=1
    for k, v in model_dict.items():
        print(k,num)
        num=num+1
        if 'num_batches_tracked' in k:
            continue
        v = v.cpu().numpy().flatten()
        for d in v:
            fp.write(d)
            weight_count+=1
    print('model_weight has Convert Completely!',weight_count)



if __name__ == '__main__':
    mnist_train = torchvision.datasets.FashionMNIST(root='Datasets/FashionMNIST', train=True, download=True, transform=transforms.ToTensor())
    mnist_test = torchvision.datasets.FashionMNIST(root='Datasets/FashionMNIST', train=False, download=True, transform=transforms.ToTensor())
    batch_size = 1
    train_iter, test_iter = load_data_fashion_mnist(mnist_train, mnist_test, batch_size)
    raw_train,raw_test = raw_data()

    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    model = LeNet().to(device)

    x = torch.as_tensor(raw_test[0][0])

    print(torch.quantize_per_tensor(x,0.5,8,torch.quint8))

    # Weight2Bin("./lenet.pth")
    # inference(model,"./lenet.pth",torch.as_tensor(raw_test[0][0]).to(device),torch.as_tensor(raw_test[0][1]).to(device))
