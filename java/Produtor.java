package semaforo;

import java.util.Random;

public class Produtor implements Runnable {
	private Drop drop;
	
	public Produtor(Drop drop) {
		this.drop = drop;
	}
	
	public void run() {
		int importantInfo[] = {1, 2, 3, 4};
		Random rd = new Random();
		for (int i = 0; i < importantInfo.length; i++) {
			drop.put(importantInfo[i]);
			try {
				Thread.sleep(rd.nextInt(500));
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		drop.put(0);
	}
}
