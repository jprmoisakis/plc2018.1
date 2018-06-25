package semaforo;

import java.util.Random;
public class Banco implements Runnable {

	private double saldo;
	
	public Banco(double saldo) {
		this.saldo = saldo;
	}
	
	public void retirar(double valor) {
		synchronized (this) {
			if (saldo - valor >= 0) {
				saldo = saldo - valor;
			} else {
				System.out.println("Saldo insuficiente");
			}
		}
	}
	
	public void depositar(double valor) {
		synchronized (this) {
			saldo = saldo + valor;
		}
	}
	
	public static void main(String[] args) throws InterruptedException {
		Banco banco = new Banco(300);
		Thread[] thread = new Thread[6];
		for (int i = 0; i < 6; i ++) {
			Thread t = new Thread(banco);
			thread[i] = t;
		}
		for (int i = 0; i < 6; i++) {
			thread[i].run();
		}
		for (int i = 0; i < 6; i++) {
			thread[i].join();
		}
	}

	@Override
	public void run() {
		Random rd = new Random();
		Double value1 = rd.nextDouble()*100;
		Double value2 = rd.nextDouble()*100;
		for (int i = 0; i < 10; i++) {
			if (i % 2 == 0) {
				retirar(value1);
			} else {
				depositar(value2);
			}
		}
		System.out.println(saldo);
	}
}
