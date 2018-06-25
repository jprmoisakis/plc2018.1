package semaforo;

import java.util.Random;
import java.util.concurrent.atomic.*;

public class BancoAtomico implements Runnable {
	
	private static AtomicInteger saldo = new AtomicInteger(0);
	
	public BancoAtomico(int dinheiro) {
		saldo.set(dinheiro);
	}
	
	public void retirar(int valor) {
		int aux = saldo.get();
		if (aux - valor < 0) {
			System.out.println("Saldo insuficiente");
		} else {
			saldo.set(aux - valor);
		}
	}
	
	public void depositar(int valor) {
		int aux = saldo.get();
		saldo.set(aux + valor);
	}

	@Override
	public void run() {
		Random rd = new Random();
		int value1 = rd.nextInt(100);
		int value2 = rd.nextInt(100);
		for (int i = 0; i < 10; i++) {
			depositar(value1);
			retirar(value2);
		}
		System.out.println(saldo.get());
	}
	
	public static void main(String[] args) throws InterruptedException {
		BancoAtomico banco = new BancoAtomico(300);
		Thread[] pessoas = new Thread[6];
		for (int i = 0; i < 6; i++) {
			Thread t = new Thread(banco);
			pessoas[i] = t;
		}
		for (int i = 0; i < 6; i++) {
			pessoas[i].run();
		}
		for (int i = 0; i < 6; i++) {
			pessoas[i].join();
		}
	}
	
}
