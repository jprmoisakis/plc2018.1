package semaforo;

import java.util.Random;

public class Consumidor implements Runnable {
	private Drop drop;
	
	public Consumidor(Drop drop) {
		this.drop = drop;
	}
	
	public void run() {
		Random rd = new Random();
		for (int message = drop.take(); message != 0; message = drop.take()) {
			System.out.format("Mensagem recebida: %s%n", message);
			try {
				Thread.sleep(rd.nextInt(500));
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		System.out.println("Mensagens lidas");
	}
}
