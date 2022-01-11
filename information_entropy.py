from math import log


def information_entropy_calculus(prob):
    """
    :param prob: probabilities of an event
    :return: information theory -- the uncertainty contained in a probability distribution
    """
    ie = -sum(i * log(i) if i > 0 else 0 for i in prob)
    print(f'Information Entropy is {ie:.04f}')
    return ie


def kl_divergence(prob1, prob2):
    kl = sum(prob1[i] * log(prob1[i]/prob2[i])
             if (prob1[i] > 0) and (prob1[i] > 0) else 0
             for i in range(len(prob1)))
    print(f'Information Entropy is {kl:.04f}')
    return kl


if __name__ == '__main__':
    p = [1, 0]
    print('Maximum entropy: least surprising. You know for sure')
    information_entropy_calculus(p)
    p = [.5, .5]
    information_entropy_calculus(p)
    p = [1/100] * 100
    information_entropy_calculus(p)

    print('Divergence')
    p = [.3, .7]
    q = [.1, .9]
    kl_divergence(p, q)

